       subroutine autfun(mf,nx,xv,ffit,iend)
c
c   evaluate a function for lmdif1, a non-linear least squares
c   levenburg-marquardt algorithm.
c
c   evaluate the difference between the fft of the theory and the
c   post-background-subtraction chi data for changing b-spline
c   coefficients.
c
c   xv(1) to xv(nsplin) are the b-spline coefficients
c
c   also allow e0 shift of the data and modification of
c   theory by overall amplitude and s02.

c
c   copyright 1992  university of washington :          matt newville
c
       include 'autobk.h'
       integer   ifft, nx, mf, iend, nvtmp, i
       integer   nqmin, nqmax, nmaxx, ipos
       double precision small, half, widmin, bvalue, sumsqr
       parameter (half = 0.5d0, small = 1.d-10, widmin = 0.5d0)
       parameter (ifft = 1)
       double precision xv(nx), ffit(mf), qtmp(maxpts), chie(maxpts)
       double precision chifit(maxpts), qtt, e0
       external  bvalue, sumsqr
c
       if (nx.ne.nvarys) iend = 1
       if (mf.ne.mfit)   iend = 2
       nvtmp  = nvarys
c
c  unwrap list of possible variables:
c     the last on the list from autnls is the first off
       if (theory.and.eevary) then
          e0shft = xv(nvtmp)
          nvtmp  = nvtmp - 1
       end if
       if (nvtmp.ne.nsplin) iend = 3
c
c  do e0 shifting to temporary q-array for interpolation,
c  evaluate the spline and normalize to get chie
c  normalize
       e0     = ee + e0shft
cc       print*, ' -------------------'
cc       print*, ' nvarys = ', nx, nvarys, mf
cc       print*, ' varys = ', xv(1), xv(2), xv(3), xv(4), xv(5)
       do 200 i = 1, nxmu
          qtmp(i)   =  sqrt(etok * abs(energy(i)-e0) )
          if (energy(i).lt.e0)   qtmp(i) = - qtmp(i)
          if ( (energy(i).le.emax).and.(energy(i).ge.emin) ) then
             spline(i) = bvalue(eknot,xv,nsplin,korder,energy(i),0)
             chie(i)   = ( xmudat(i) - spline(i) ) / step
             if (funnrm)
     $            chie(i)   = ( xmudat(i) /max(spline(i), small)) - one
          else
             chie(i)   = zero
          end if
 200   continue
c  get qmax and qmin from the energy values
       nqmin = int( sqrt( etok* abs(emin - e0) ) / qgrid )
       nqmax = int( sqrt( etok* abs(emax - e0) ) / qgrid )
       if ((emin.lt.e0).or.(nqmin.lt.1))     nqmin = 1
c  interpolate chiq to q grid, evaluate chiq, the data chi
       nmaxx  = min(maxpts, nqmax + 20)
       ipos   = 1
cc       print*, ' autfun ', nqmax, nmaxx, nqmin, emin, emax, nterp
       do 300 i = 1, nmaxx
          if ( (i.lt.nqmin).or.(i.gt.nqmax) ) then
             chiq(i) = zero
          else
             qtt = (i-1) * qgrid
             if (final) then 
                if (nterp.eq.1) then
                   call lintrp(qtmp, chie, nxmu, qtt, ipos, chiq(i))
                else 
                   call qintrp(qtmp, chie, nxmu, qtt, ipos, chiq(i))
                end if
             else
                call qintrp(qtmp, chie, nxmu, qtt, ipos, chiq(i) )
             end if
          end if
 300   continue
c get real and imaginary parts of the fft of data chiq
cc       print*, 'autfun qgrid = ', qgrid, r1st, ifft, nrbkg, theamp
       call fitfft(chiq, maxpts, mftfit, wfftc, qgrid,
     $      windo, qweigh, windo, one, ifft, zero, r1st,
     $      nrpts, chifit)
cc       print*, ' mpts: = ', mftfit, maxpts, nrpts
c     
c find amplitude scale for theory so that it matches the 
c data over the first shell.  this sets  theamp to the ratio 
c of the power-spectral-densities of the first shell xafs for 
c the theory and data chiq. 
       if (.not.thefix)  theamp = sumsqr(chifit(nrbkg),nr1st) /
     $                            thessq

c there are nrbkg points in the low-r region
c there are nr1st points in the first shell region
c there are nrpts points total
c add r-weighting to bkg removal. 
c note that rwgt is converted to integer, and that the real and 
c imaginary portions are weighted equally by using integer arithmetic.
cc       print*, chifit(1), chifit(7), chifit(17), chifit(22)
       do 550 i = 1, nrbkg
          ffit(i) = chifit(i) - thifit(i) * theamp
 550   continue
       return
c end subroutine autfun
       end
