       subroutine spline(energy, xmu, mxmu, xkstd, chistd, mstd,
     $      e0_in, rbkgin, r1stin, toler, nknots, 
     $      qmin, qmax, xkw, dk1, dk2, winnam, 
     $      stfind, fnorm, enor1, enor2, pre1, pre2, estep,
     $      lclmp1, xclmp1, lclmp2, xclmp2, nclmp,
     $      cnorm, fixstd, usestd, varye0, spstep,
     $      dofit, bkg, mxk, xk, chi)
c
c//////////////////////////////////////////////////////////////////////
c Copyright (c) 1997--2000 Matthew Newville, The University of Chicago
c Copyright (c) 1992--1996 Matthew Newville, University of Washington
c
c Permission to use and redistribute the source code or binary forms of
c this software and its documentation, with or without modification is
c hereby granted provided that the above notice of copyright, these
c terms of use, and the disclaimer of warranty below appear in the
c source code and documentation, and that none of the names of The
c University of Chicago, The University of Washington, or the authors
c appear in advertising or endorsement of works derived from this
c software without specific prior written permission from all parties.
c
c THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
c EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
c MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
c IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
c CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
c TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
c SOFTWARE OR THE USE OR OTHER DEALINGS IN THIS SOFTWARE.
c//////////////////////////////////////////////////////////////////////
c
c purpose: automated xafs background spline (the autobk algorithm)
c
c arguments:
c      energy   array of energy values                  [in]
c      xmu      array of xmu values                     [in]
c      mxmu     maximum array length of energy/xmu      [in]
c      xkstd    array of k values for standard          [in]
c      chistd   array of chi values for standard        [in]
c      mstd     maximum array length of xkstd/chistd    [in]
c      fixstd   l=flag for fixing amp of standard       [in]
c      usestd   l=flag for using standard at all        [in]
c      rbkgin   rbkg value (nyquist value)              [in/out]
c      toler    fitting tolerance                       [in/out]
c      nknots   number of knots in spline               [in/out]
c      qmin     low-k value for fft window              [in/out]
c      qmax     high-k value for fft window             [in/out]
c      xkw      k-weight for fft                        [in]
c      dk1      low-k value for fft window              [in]
c      dk2      low-k value for fft window              [in]
c      winnam   fft window type                         [in]
c      varye0   l=flag for varying e0 in fit            [in]
c      e0_in    energy origin                           [in/out]
c      stfind   l=flag for redefining estep             [in]
c      fnorm    l=flag for functional normalization     [in]
c      enor1    low-energy  range for post-step         [in]
c      enor2    high-energy range for post-step         [in]
c      pre1     low-energy  range for pre-edge          [in]
c      pre2     high-energy range for pre-edge          [in]
c      estep    edge-step, aka delta_xmu                [in/out]
c      dofit    l=flag for doing fit at all             [in]
c      bkg      array of mu0 values   (mxmu points)     [out]
c      mxk      maximum array length of xk/chi          [out]
c      xk       array of k values                       [out]
c      chi      array of chi values                     [out]
c
c notes:
c   1. for use in ifeffit: uses ifeffit include files
c   2. based on/related to suroutine autnls from autobk
c   3. not well tested with standard chi(k) data
c
c see documentation and code comments for more details
c
c requires:  include files  consts.h, spline.h, fft.h
c            splfun, lmdif1, cffti, qintrp, lintrp, polyft,
c            fitfft, window, sumsqr, nofx, echo, chrdmp
c
c
       implicit none
       double precision  energy(*), xmu(*), bkg(*), xkstd(*), chistd(*)
       double precision  xk(*), chi(*), enor1, enor2, e0_in, estep
       double precision  rbkgin, toler,qmin,qmax
       double precision  xkw,dk1,dk2, pre1, pre2, cnorm(*)
       integer mxmu, mxk, mstd, nknots, nx1
       logical fnorm, fixstd, usestd, varye0, stfind, dofit
       logical           lclmp1, lclmp2
       double precision  xclmp1, xclmp2
c
       include 'consts.h'
       include 'spline.h'
       include 'fft.h'
c
       integer lenwrk, loop, lminfo, iup, ilo, nnorm , iterp
       integer iwork(mtknot), nemin, nemax, nofx, nrfit, nclmp
       integer i, nr, iend,  ntest, ipos, nqmax, j
       parameter(lenwrk = 2*maxpts*mtknot + 20*mtknot + 2*maxpts   )
       character*80  messg, winnam*32
       double precision   work(lenwrk), fvect(2*maxpts), varys(mtknot)
       double precision   esplin(mtknot), bscoef(mtknot), thiq(maxpts)
       double precision   small, toldef, spstep, r1stin
       double precision   sumsqr, qmn, qmx, qknot, slope, offset
       double precision  e0tst, qmntst, qmxtst,  xtmp, qtmp, e0t, tmp
       parameter (small  = 1.d-10, toldef = 1.d-5)
       external splfun, nofx, sumsqr
       save
c
       nnorm  = 3
       if (toler .le. zero) toler = toldef
       spstep = min(20.d0, max(1.d-6, spstep))
       loop   = 0

       if (.not.wftset) then
          call cffti(maxfft, wfftc)
          wftset = .true.
       end if

       rbkg   = rbkgin
       r1st   = rbkg + r1stin
       if (rbkg .le. small) rbkg = one
       nrbkg  = 2 * int(1d-2 + (rbkg /rgrid))+ 2
       nr1st  = 2 * int(1d-2 + (r1st /rgrid))+ 2
       if (qmax .le. small) qmax = 0
c
c initialize bkgdat common block
       splqw = xkw
       nsplin = nknots
       nautbk = 0
       thefix = fixstd
       theory = usestd
       eevary = varye0 .and. theory
cc       print*, ' spline:  eevary, = ', eevary, theory
       step   = estep
       e0     = e0_in
       emin   = e0 + qmin**2 / etok
       emax   = e0 + qmax**2 / etok
       e0t    = e0
       de0    = 0
       thessq = one
       thebkg = one
       nxmu   = mxmu
       do 10 i = 1, maxpts
          endat(i)  = zero
          xmudat(i) = zero
          spldat(i) = zero
          chie(i)   = zero
          splfit(i) = zero
          splwin(i) = zero
 10    continue
       do 20 i = 1, nxmu
          endat(i)  = energy(i)
          xmudat(i) = xmu(i)
          spldat(i) = xmu(i)
 20    continue

c do xafsft window
c put standard chi(k) on an absolute q-grid, and do fft
       if (mstd.gt.0) then
          nqmax = int( xkstd(mstd) / qgrid)
          ipos  = 1
          do 30 i = 1, nqmax
             qtmp = qgrid * (i-1)
             call qintrp(xkstd, chistd, mstd, qtmp, ipos, thiq(i))
 30       continue
       end if
c
 100   continue
       lminfo = 0
       loop   = loop + 1
c
c----start fitting:
       do 120 i = 1, nxmu
          bkg(i) = spldat(i)
 120   continue
c
       clamp(1)  = lclmp1
       clamp(2)  = lclmp2
       sclamp(1) = xclmp1
       sclamp(2) = xclmp2
       nclamp    = nclmp
cc       print*,' clamps: ', clamp, sclamp, nclamp
c
c  initialize fvect and work arrays for lmdif1

       nrfit = nrbkg
       if (theory)   nrfit = nr1st
       if (clamp(1)) nrfit = nrfit + nclamp
       if (clamp(2)) nrfit = nrfit + nclamp
       do 210 i =1, nrfit
          fvect(i) = zero
 210   continue
       do 220 i =1, lenwrk
          work(i) = zero
 220   continue
       do 240 i =1, mtknot
          iwork(i)  = 0
          varys(i)  = zero
          esplin(i) = zero
          bscoef(i) = zero
 240   continue
c
c  e0-shift:  e0, emin, and emax
       if (abs(de0).gt.1d-3) then
          e0t  = e0t  + de0
          emin = emin + de0
          emax = emax + de0
       end if
c
       if (emax.le.emin) emax = energy(nxmu)
       nemin = nofx(emin,energy,nxmu)
       nemax = nofx(emax,energy,nxmu)
       emin  = max(e0t, energy(nemin))
       emax  = max(emin, energy(nemax))
c
c  evaluate energies for the spline variables:
c  - emin and emax are not relative to the edge
c  - energy contains the input energy values, not relative to the edge.
       qmin   = sqrt(etok* abs(emin - e0t) )
       qmax   = sqrt(etok* (emax - e0t) )
c
c calculate number of independent points in r-space
       if (nsplin.le.1)
     $      nsplin = 2 * int(rbkg * (qmax - qmin)/ pi)  +  1
       nsplin = min(mtknot-5, max(5,nsplin))
       nknots = nsplin
cc       print*, ' SPLINE ', rbkg, nsplin

c  initialize energy values through which the first guess for the
c  spline must go (evenly spaced in q), and get the initial value
c  for the spline value at this point. we'll also get the initial
c  guesses for the variables (the b-spline coefficients) from this
c  initial spline.
       do 300 i = 1, nsplin
          qtmp      = qmin + (i-1)*(qmax-qmin)/(nsplin - 1)
          xtmp      = e0t + ( qtmp**2 / etok )
          j         = nofx(xtmp,energy,nxmu)
          esplin(i) = energy(j)
          iup       = min(nxmu, j + 5)
          ilo       = max(1,    j - 5)
          bscoef(i) = (2*spldat(j)+ spldat(iup)+spldat(ilo))/4
300    continue
       esplin(nsplin) = one + esplin(nsplin)
c the first and last korder knots in the b-spline are nearly degenerate at
c the endpoints. spstep sets the spacing between these points.  the
c default is one -- this may help eliminate "spikes" at the endpoints.
c since each knot represents a place where a derivative can break,
c  having all four of these at one place allows a complete break from
c at this point. by moving a few of the knots just off the ends, the
c spline is a little bit stiffer at the endpoints.
cc       print*, ' spline : ', spstep, korder
       do 310 i = 1, korder
           eknot(i)        = esplin(1)      - spstep * (korder-i-1)
           eknot(nsplin+i) = esplin(nsplin) + spstep * i
310    continue
       qmn       = sqrt( etok* abs(esplin(1) - e0t) )
       if (e0.lt.esplin(1))  qmn = zero
       qmx = sqrt( etok* (esplin(nsplin) - e0t) )
       do 320 i = korder+1, nsplin
           qknot    = (i-korder)*(qmx - qmn)/(nsplin-korder+1)
           eknot(i) = esplin(1) + qknot**2/etok
320    continue
c
c    determine the knots for the spline:
c    knots are points at which the spline has extra freedom.
       if ( (korder.lt.3).or.(nsplin.lt.korder) ) then
          call warn(2,
     $         ' spline error: not enough data to create spline.')
          return
       end if
c  the b-spline coefficients will be the variables in the fit,
c  the above estimates for the elements of bscoef are good enough
c  especially when the spline values are smoothed for the initial
c  guesses in the fit, as below.
       varys(1) = (3*bscoef(1) + bscoef(2))/ 4
       varys(nsplin) = (3*bscoef(nsplin) + bscoef(nsplin-1))/ 4
       do 380 i = 2, nsplin-1
          varys(i) =  (bscoef(i-1) + 2*bscoef(i) + bscoef(i+1) )/4
 380   continue
       nautbk = nsplin
c
c set up window function
       call window(winnam,dk1,dk2,qmin,qmax,qgrid,maxpts,splwin)
c  if a theory file is used, do its fft now
c  and add another variable if e0 is to be shifted
       if (theory) then
          if (eevary) then
             nautbk        = nautbk + 1
             varys(nautbk) = de0
          end if
          nr  = nrbkg
          call fitfft(thiq, maxpts, maxfft, wfftc, qgrid,
     $         splwin, splqw, bkg, zero, 1, zero, r1st,
     $         nr, splfit)
          thessq = max(small, sumsqr(splfit(nrbkg),nr1st))
          thebkg = thessq
cc          print*, ' spline.f: thebkg = ', thebkg
cc          print*, '   nrbkg, nr1st   = ', nrbkg, nr1st, nr
          nautbk        = nautbk + 1
          varys(nautbk) = thessq
cc
          if (nr.ne.nr1st)  then
             call warn(3,' spline error: fitfft is broken' )
             return
          end if
       end if
c
       if (eevary) then
          call chrdmp(' spline: fitting background and e0 ... ')
       else
          call chrdmp(' spline: fitting background ... ')
       end if
c
       lminfo = 1
       if (dofit) then
          call lmdif1 (splfun, nrfit, nautbk, varys, fvect,
     $         toler, lminfo, iwork, work, lenwrk)
          call echo(' done.')
          call lm_err(lminfo,toler)
       end if
c
c  if an energy shift was done, the q values may be slightly off, so that
c  the number of independent points changes.  if so, re-do the fit until
c  e0 is stable to 0.5 eV, up to 5 times
       e0tst   = e0 + de0
       qmntst  = sqrt(etok* abs(emin - e0tst ) )
       qmxtst  = sqrt(etok* abs(emax - e0tst ) )
       if (e0tst.le.emin)    qmntst   = zero
       ntest   = 2 * int ( rbkg * abs(qmxtst - qmntst) / pi)  + 1
       ntest   = min(mtknot-5, max(5,ntest))
c
       if ((loop.le.5).and.eevary.and.(ntest.ne.nsplin) ) then
          call echo(' spline warning:  e0 was shifted enough that')
          call warn(1, ' the # of knots in the spline changed'//
     $         ' and the fit should be re-done.')
          go to 100
       end if
c
c  now that we have a good spline, we may want to improve the estimate
c  of the edge-step. so do a parabolic extrapolation of the background
c  spline to the edge energy value, which is probably better than
c  getting the edge step from a linear extrapolation of the xmu data.
c redo function evaluation with the final values for everything.
       call splfun(nrfit, nautbk, varys, fvect, iend)
       if (stfind.and.(.not.fnorm)) then
          call echo(' spline: finding edge_step from bkg(E)')
          call preedg(.false.,.true.,nxmu, endat, bkg, e0,
     $         pre1, pre2, enor1, enor2, nnorm,
     $         step, slope, offset,cnorm)
          estep = step
       end if
c redo function evaluation with the final values for everything.
       call splfun(nrfit, nautbk, varys, fvect, iend)
       if (eevary)  then
          e0    = e0_in + de0
          e0_in = e0
       end if
       do 830 i = 1, nxmu
          bkg(i) = spldat(i)
 830   continue

c set chi array
       call chie2k(endat,chie,nxmu,e0, mxk,xk,chi)
c       print*, 'MM end of spline ', endat(1), endat(2),
c     $      chie(1), chie(2), xk(1), xk(2),
c     $      chi(1), chi(2), nxmu, e0, mxk 
c end  subroutine spline
       return
       end



