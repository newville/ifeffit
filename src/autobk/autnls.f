       subroutine autnls
c
c   this prepares for and calls the canned subroutine lmdif1
c   which will solve the unconstrained non-linear least squares
c   fitting problem. lmdif1 uses a levenberg-marquardt algorithm,
c   and requires an external subprogram to evaluate the function
c   to minimize. the subroutine autfun is used to evaluate this
c   function.
c   the function allows the ordinates of the breakpoints of
c   the spline to vary such that when the optimal b-spline (as
c   discussed in de boor) is put through these breakpoints, and
c   the chi(r) is found using this spline as the background is
c   optimized at low-r.
c
c   copyright 1992  university of washington :          matt newville
c------------------------------------------------------------------
       include 'autobk.h'
c------------------------------------------------------------------
       integer   lenwrk, loop, lminfo, isplin, iup, ilo
       integer   iwork(mtknot), ifft, nemin, nemax, nofx, ne0
       integer   i, nr, ndoc, iend, im, istrln, ntest
       parameter(lenwrk = 2*maxpts*mtknot + 20*mtknot + 2*maxpts   )
       character*128  messg , doc(maxdoc),  type*10,  file*40
       double precision work(lenwrk),  fvect(2*maxpts), varys(mtknot)
       double precision esplin(mtknot), bscoef(mtknot)
       double precision etmp(maxpts), small, anorm(3), toler, tolfac
       double precision sumsqr
       double precision e0, delq, efromq, qmn, qmx, qknot
       double precision e0tst, qmntst, qmxtst, edfmin
       parameter (small  = 1.d-10, tolfac = 1.d-4, edfmin= 100.d0)
       external autfun, nofx, istrln, sumsqr
c-----------------------------------------------------------------------
c----initialization for fitting:
c   -the starting value for the fitting tolerance (1e-5) is empirical.
c    the user will be allowed to change it, if necessary, with usertl.
c    (right now usertl is set to 1).
c   -the fitting is done in single precision. round off is a worry, but
c    i have not been able to get the variables to change enough with the
c    double precision version of lmdif1. this should still be explored.
c-----------------------------------------------------------------------
       toler  = usrtol * tolfac
       e0     = ee + e0shft
       ifft  =  1
       loop   = 0
 100   continue
       lminfo = 0
       loop   = loop + 1
       if (loop.gt.5) then
          call echo('   autobk warning: completed 5'// 
     $         ' fitting loops without a stable result.')
          call echo('                   something may be wrong'//
     $         ' with the fitting,')
          call echo('                   and the background may'//
     $         ' not be reliable.')
          go to 800
       end if
c----start fitting:
       do 120 i = 1, nxmu
          spline(i) = xmudat(i)
 120   continue 
       e0    = ee  + e0shft
c   move emin and emax if e0-shifted
       emin  = emin + e0shft
       emax  = emax + e0shft
       if (emax.le.emin) emax = energy(nxmu)
       nemin = nofx(emin,energy,nxmu)
       nemax = nofx(emax,energy,nxmu)
       emin  = energy(nemin)
       emax  = energy(nemax)
c  evaluate energies for the spline variables:
c  - emin and emax are not relative to the edge
c  - energy contains the input energy values, not relative to the edge.
       qmin   = sqrt(etok* abs(emin - e0 ) )
       if (e0.le.emin)    qmin   = zero
       qmax   = sqrt(etok* (emax - e0 ) )
c     
c drpair is the spacing between pairs of independent points in r-space 
       if (.not.gvknot)
     $      nsplin = 2 * int ( rbkg * ( qmax - qmin ) / pi )  +  1 
       nsplin = min(mtknot-5, max(5,nsplin))
c     
c  initialize energy values through which the first guess for the
c  spline must go (evenly spaced in q), and get the initial value
c  for the spline value at this point. we'll also get the initial
c  guesses for the variables (the b-spline coefficients) from this
c  initial spline.
c
       delq = (qmax-qmin)/(nsplin - 1)
       do 300 i = 1, nsplin
           efromq    = e0 + ( qmin + (i-1) * delq) **2 / etok
           isplin    = nofx(efromq,energy,nxmu)
           esplin(i) = energy(isplin)
           iup       = min(nxmu, isplin + 5)
           ilo       = max(1,    isplin - 5)
           bscoef(i) = (2*spline(isplin) + spline(iup) + spline(ilo))/4
300    continue
       esplin(nsplin) = one + esplin(nsplin)
c the first and last korder knots in the b-spline are nearly degenerate at
c the endpoints. spstep sets the spacing between these points.  the 
c default is one -- this may help eliminate "spikes" at the endpoints.
c since each knot represents a place where a derivative can break,
c  having all four of these at one place allows a complete break from 
c at this point. by moving a few of the knots just off the ends, the 
c spline is a little bit stiffer at the endpoints.
       do 310 i = 1, korder
           eknot(i)        = esplin(1)      - spstep * (korder-i-1)
           eknot(nsplin+i) = esplin(nsplin) + spstep * i
310    continue
       qmn       = sqrt( etok* abs(esplin(1) - e0) )
       if (e0.lt.esplin(1))  qmn = zero
       qmx = sqrt( etok* (esplin(nsplin) - e0) )
       do 320 i = korder+1, nsplin
           qknot    = (i-korder)*(qmx - qmn)/(nsplin-korder+1)
           eknot(i) = esplin(1) + qknot**2/etok
320    continue

c
c    determine the knots for the spline:
c    knots are points at which the spline has extra freedom.
cc       ntknot = nsplin + korder
       if ( (korder.lt.3).or.(nsplin.lt.korder) ) then
          call echo('           autobk error: not enough'//
     $                      ' freedom to create spline.')
          call echo('                   change fitting range, or'//
     $                      ' order of spline')
          call fstop(' not enough data to make spline')
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
       nvarys = nsplin
c set up window function
       call window(winstr,windo1,windo2,qmin,qmax,qgrid,maxpts,windo)
c  if a theory file is used, do its fft now
c  and add another variable if e0 is to be shifted
       if (theory) then 
          if (eevary) then
             nvarys        = nvarys + 1
             varys(nvarys) = e0shft
          end if
          nr     = nrpts
cc          print*, ' theory : ', mftfit
          call fitfft(thiq, maxpts, mftfit, wfftc, qgrid,
     $         windo, qweigh, spline, one, ifft, zero, r1st,
     $         nrpts, thifit)

          thessq = max(small, sumsqr(thifit(nrbkg),nr1st))
          if (nr.ne.nrpts)  then
             call echo('  autnls error: fitfft is broken' )
             write(messg,'(a,2i5)') 'nr, nrpts = ', nr, nrpts
             call echo('          '//messg(:40))
             call fstop(' call matt!! autobk fitfft is broken')
          end if
       end if
c 
c  initialize fvect and work arrays for lmdif1
       mfit  = nrbkg
       do 400 i =1, mfit
          fvect(i) = zero
 400   continue
       do 410 i =1, lenwrk
          work(i) = zero
 410   continue
       do 420 i =1, mtknot
          iwork(i) = 0
 420   continue
c     
c  lmdif1 to do levenberg-marquardt nonlinear least squares
       if (eevary) then
          call echo('   autobk: fitting background and e0'//
     $         ' over the low-r range')
       else
          call echo('   autobk: fitting background'//
     $         ' over the low-r range')
       end if
       if (iprint.ge.2) then 
          iend = 0
cc          print*, ' call autfun: ', mfit, nvarys, iend
cc          print*, varys(1), fvect(1), varys(3), fvect(3)
          call autfun(mfit, nvarys, varys, fvect, iend)
          type   = 'xmu'
          file   = 'spline0.dat'
          doc(1) = ' autobk: initial spline v. e : before fitting'
 500      format (1x,a,i4)
 501      format (1x,a)
 502      format (1x,i5.2,e14.6,e14.6,1x,i5.2,e14.6,e14.6)
 503      format (1x,i5.2,e14.6,e14.6)
 504      format (1x,a,f14.6)
          write (doc(2), 500) ' number of variables = ', nsplin
          write (doc(3), 504) ' spline_step (spstep)= ', spstep
          write (doc(4), 501) ' index, esplin, ysplin, '//
     $                        ' index, esplin, ysplin '
          ndoc   = 4
          do 550 i = 1, nsplin, 2
             if (i.lt.maxdoc)  ndoc = ndoc + 1
             if ((i.le.maxdoc).and.(i.eq.nsplin)) then
                write (doc(ndoc), 503) i , esplin(i)  , varys(i)  
             elseif (i.le.maxdoc) then
                write (doc(ndoc), 502) i  , esplin(i)  , varys(i)  ,
     $                                i+1, esplin(i+1), varys(i+1)
             end if
 550      continue 
          call outcol(type, file, asccmt, ndoc, mdocxx, doc, nxmu,
     $         energy, spline, xmudat, chiq, thiq) 
       end if
       
c
c the real fit!
       call lmdif1 (autfun, mfit, nvarys, varys, fvect,
     $      toler, lminfo,  iwork,  work, lenwrk)

       call autfun(mfit, nvarys, varys, fvect, iend)
       if ( (lminfo.ge.1).and.(lminfo.le.3) ) then
          messg = 'fitting is finished.'
          im = max(1, istrln(messg))
          call echo ('                   '//messg(:im) )
       else
          messg = 'lmdif finished with an error ! '
          im = max(1, istrln(messg))
          call echo ('                   '//messg(:im) )
          write(messg, '( a, i5)' ) 'error code lminfo = ',lminfo
          im = max(1, istrln(messg))
          call echo ('                   '//messg(:im) )
          call echo ('            call matt')
       end if
c
       if (iprint.ge.2) then 
          file   = 'spline.dat'
          doc(1) = ' autobk: final spline v. e : before fitting'
          ndoc   = 4
          do 580 i = 1, nsplin, 2
             if (i.lt.maxdoc)  ndoc = ndoc + 1
             if ((i.le.maxdoc).and.(i.eq.nsplin)) then
                write (doc(ndoc), 503) i , esplin(i)  , varys(i)  
             elseif (i.le.maxdoc) then
                write (doc(ndoc), 502) i  , esplin(i)  , varys(i)  ,
     $                                i+1, esplin(i+1), varys(i+1)
             end if
 580      continue 
          call outcol(type, file, asccmt, ndoc, mdocxx, doc, nxmu,
     $         energy, spline, xmudat, chiq, thiq) 
       end if


c  even though the fit was good, if an energy shift was done, the
c  q values used to determine the number and location of the spline
c  points were wrong, and so the fit must be re-done. this will be
c  repeated up to 5 times, or until e0 is stable to within 0.5 ev.
       e0tst   = ee + e0shft
       qmntst  = sqrt(etok* abs(emin - e0tst ) )
       qmxtst  = sqrt(etok* abs(emax - e0tst ) )
       if (e0tst.le.emin)    qmntst   = zero
       ntest   = 2 * int ( rbkg * abs(qmxtst - qmntst) / pi)  + 1 
       if (gvknot) ntest = nsplin
       ntest   = min(mtknot-5, max(5,ntest))
       if (eevary.and.(ntest.ne.nsplin) ) then
          call echo('   autobk warning: because the'//
     $                ' energy origin was shifted in ')
          call echo('                   in the fit, kmin'//
     $                ' and kmax have been redefined')
          call echo('                   so that the number'//
     $                ' of knots in the spline has')
          call echo('                   changed. the fit '//
     $                ' will be done again.')
          go to 100
       end if
c------------------------------------------------------------------
c  finished with fit
 800   continue
c
c  now that we have the spline as good as we can get it, let's get an 
c  improved estimate of the edge-step. we should have it pretty close 
c  to one, but now we do a parabolic extrapolation of the background 
c  spline to the edge energy value. this should cover up most 
c  difficulties in getting the edge step from a linear extrapolation
c  of the absorption data itself.
       if (stfind.and.(.not.funnrm)) then
          enor1  = e0 + enor1
          enor2  = e0 + enor2
          if (enor2.gt.energy(nxmu))   enor2 = energy(nxmu) 
          if (enor1.gt.energy(nxmu))   enor1 = enor2 /2
          nnorm = 3
          if (abs(enor2 - enor1).le.edfmin) nnorm = 2
          call polyft(enor1, enor2, energy, spline,nxmu,nnorm,cnorm)
          ne0  = nofx(e0, energy, nxmu)
          step = cnorm(1) + cnorm(2)*energy(ne0)
          if (nnorm.eq.3) step = step + cnorm(3)*energy(ne0)**2
          cnorm(1) = cnorm(1) + bpre 
          cnorm(2) = cnorm(2) + slopre
       end if
c
c  now redo function evaluation with the final values for everything.
       do 930 i = 1, nxmu
          spline(i) = xmudat(i)
 930   continue
       do 940 i = 1, maxpts
          chiq(i) = zero
 940   continue
       final = .true.
       call autfun(mfit, nvarys, varys, fvect, iend)
c     
       return
c end subroutine autnls
       end
