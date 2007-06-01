       subroutine fitnls
c
c    this will call lmdif1, a routine from minpack, to solve the
c    unconstrained non-linear least squares fitting problem using
c    a levenberg-marquardt algorithm. the subroutine fitfun is
c    used to evaluate the function to be minimized in the least
c    squares sense.
c
c      copyright 1993 university of washington         matt newville
c
c    the outline of this routine is:
c    1.  an estimate of the measurement uncertainty is made by
c        using the high r components of the data (unless the user
c        has overwritten this value).
c    2.  the arrays for the call to lmdif1 are setup, and the call
c        to lmdif1 is made.
c    3.  after a successful fit has been found, the uncertainties
c        in the fit parameters are estimated by assuming that the
c        errors are normally distributed, so that the correlation
c        matrix is the inverse of the curvature of parameter space
c        around the solution.
c
c--------------------------------------------------------------------
       include 'fitcom.h'

       integer lenwrk,  lenfvc, istrln, im, ier, ierr, ilen, ione
       integer lminfo, iflag, nfirst, nr1, nr2, mfit, nsigd, ix
       integer nrwght, irun, iex, id, i, istop, nrmin,nrmax, ind
       parameter(lenwrk = 2*maxpts*(mvarys + 1)  + 20*mvarys )
       parameter(lenfvc = mdata*maxpts , ione = 1)
       integer   iwork(mvarys),nptfit(mdata),ibadx(mvarys),nfit1,nsig1
       logical   datafl, feff
       double precision work(lenwrk), fvect(lenfvc), ftemp(lenfvc)
       double precision xvarys(mvarys), fjac(lenfvc, mvarys)
       double precision alpha(mvarys, mvarys), toler, tolfac
       double precision chirhi(maxpts), sumsqr
       double precision rsmall, stmp, wtmp, xolow, xohigh
       external fitfun, sumsqr, istrln

       datafl = .false.
       tolfac = 1.d-05
       lminfo = 0
       rsmall  = rgrid * 0.01d0
c-----------------------------------------------------------------
c for phase-shifted FT's, determine which feff path to take
c the phase shift from
       if (pcout) then
          do 40 id  = 1, ndata
             jffphs(id) = 0
             do 30 ind  = 1, mdpths
                if (jdtpth(ind,id).gt.0) then
                   jffphs(id) = jpthff( jdtpth(ind,id) )
                   if (jffphs(id).gt.0) go to 35
                end if
 30          continue 
 35          continue 
 40       continue 
       end if
       if (iprint.ge.2) then
          irun = 0
          call openfl(irun, 'feffit.run','unknown', iex, ierr)
          if (ierr.lt.0) then
             call finmsg(1002,' ','feffit.run',0)
             iprint = 1
          else
             write(irun,*)'mftfit, rgrid =',mftfit,rgrid
          end if
       end if
c
c hack by matt : ifxvar temporarily holds irun (so iprint works in fitfun)
       ifxvar = irun
c
c set up/initialize a bunch of other stuff:
c   mfit   = number of point to use in fitting
c   nrwght = # of points to use for getting measurement uncertainty
       mfit    = 0
       nr1    = int ( (rwght1 + rsmall) / rgrid )
       nr2    = int ( (rwght2 + rsmall) / rgrid )
       nrwght = nr2 - nr1 + 1
       nsigd  = 2*nrwght
c
c for each data set:
c  1 call fitfft to apply window and weighting and to do fft
c    of the data, returning chifit over the given fit range.
c    if ifft=0, chifit contains real chi(k), with only window
c               applied, but no fft.
c    if ifft=1, chifit contains chi(r). this is the default
c    if ifft=2, chifit contains backtransformed chi(k).
c  2 calculate the measurement uncertainty by calling fitfft
c    (this time doing the fft even if the fit is in k-space),
c    returning chi(r) between 15 and 25 angstroms. if the fit
c    is done in r-space, the measurement uncertainty is taken
c    as the rms value of the high-r components of chi(r), which
c    will represent the random, white noise in the data. if the
c    fit is done in k-space, the uncertainty is found from the
c    r-space value according to (see thiel, livins, stern, and
c    lewis paper on pt-pop. the result is fairly straighforward
c    fourier analysis, done by p livins, checked algebraically and
c    numerically on samples of modelled noisy data by m newville.)
c
c                                 /         pi * w'            \
c    (sigma_k)^2 = (sigma_r)^2 * | --------------------------   |
c                                 \ qgrid*( kmax^w' - kmin^w') /
c
c      where  w' = (2 * kweight + 1).
c
       do 100 id = 1, ndata
          nrmin    = int ( (rmin(id) + rsmall) / rgrid )
          nrmax    = int ( (rmax(id) + rsmall) / rgrid )
          nrpts(id)= nrmax  - nrmin + 1
          if (ifft(id).eq.1) then
             nptfit(id) = 2 * max (1, nrpts(id))
          elseif (ifft(id).eq.2) then
             nptfit(id) = 2 * max (1, nqpts(id))
          else
             nptfit(id) =     max (1, nqpts(id))
          end if
          mfit   = mfit + nptfit(id)
          if (datain(id)) then
             datafl  = .true.
             if (mod(ifft(id), 2).eq.0) then
                xolow = qmin(id)
                xohigh= qmax(id)
             else
                xolow = rmin(id)
                xohigh= rmax(id)
             end if

             if (iprint.ge.2) then
                write(irun,*) ' id, ifft(id) =', id , ifft(id)
                write(irun,*) ' nrmin, nrmax = ',nrmin,nrmax
                write(irun,*) ' nrpts, nqpts = ',nrpts(id),nqpts(id)
                write(irun,*) ' qmin , qmax  = ',qmin(id),qmax(id)
                write(irun,*) ' sample of qwindo (i,q,qwindo):'                
                do ix = 20, 60,3
                   write(irun,*) ix, ix*qgrid, qwindo(ix,id)
                end do
                write(irun,*) '  calling fitfft... '
             endif
             call fitfft(chiq(1,id), maxpts, mftfit, wfftc, qgrid,
     $            qwindo(1,id),qweigh(id),rwindo(1,id),rweigh(id),
     $            ifft(id), xolow,xohigh,nfit1, chifit(1,id))
cc   thepha(1,jffphs(id)),mffpts, 
c
             if (iprint.ge.2) then
                write(irun,*) ' id, ifft(id) =', id , ifft(id)
                write(irun,*) ' nrmin, nrmax = ',nrmin,nrmax
                write(irun,*) ' nrpts, nqpts = ',nrpts(id),nqpts(id)
                write(irun,*) ' qmin , qmax  = ',qmin(id),qmax(id)
             endif
c
c estimate of measurement uncertainty for fit:
c    assuming the measurement uncertainty to be white noise, and
c    that the signal dies off appreciably at reasonably large r,
c    the noise is given by the high r components of the signal. 
c    sigdtr is estimated as the rms part of the signal at high r. 
c    we most need the noise in the real and/or imaginary parts
c    of chi(r). the temp array below contains both real and 
c    imaginary parts, so its rms is too big by the sqrt(2). 
c
             if ((sigdtr(id).le.zero).and.(sigdtk(id).le.zero)) then
                call fitfft(chiq(1,id), maxpts, mftfit, wfftc, qgrid,
     $               qwindo(1,id),qweigh(id), qwindo(1,id), rweigh(id),
     $               ione, rwght1, rwght2, nsig1, chirhi)

ccc  pcfit, qfeff(1,jffphs(id)), thepha(1,jffphs(id)),mffpts, 
                sigdtr(id) = sqrt( sumsqr(chirhi, nsig1) / nsig1)
             endif
c  find sigdtk, the measurement uncertainty for the k-space data,
c  using the formula above
c  if sigdtk was given, the sigdtr is still zero, so
c  get it by inverting the above formula
             if ((sigdtk(id).le.zero).or.(sigdtr(id).le.zero)) then
                wtmp   = 2 * qweigh(id)  + one
c#mn mar-18-98 sqrt(2) seems needed, empirically at least
                stmp   = sqrt( 2 * pi * wtmp /
     $               (qgrid * (qmax(id)**wtmp - qmin(id)**wtmp )))
c                 stmp   = sqrt ( pi * wtmp /
c      $               (qgrid * (qmax(id)**wtmp - qmin(id)**wtmp )))
                if (sigdtr(id).le.zero) sigdtr(id) = sigdtk(id)/stmp
                if (sigdtk(id).le.zero) sigdtk(id) = sigdtr(id)*stmp
             end if
          endif
          sigdtk(id) = dabs( sigdtk(id))
          sigdtr(id) = dabs( sigdtr(id))
          if (sigdtk(id).le.zero) sigdtk(id) = one
          if (sigdtr(id).le.zero) sigdtr(id) = one
c  finally, get sigdtq, again using the above formula, this
c  time using the  r-weight and r-ranges of the r->q ft
          if (sigdtq(id).le.zero) then
             wtmp   = 2 * rweigh(id)  + one
             stmp   = sqrt ( pi * wtmp /
     $            (rgrid * (rmax(id)**wtmp - rmin(id)**wtmp )))
             sigdtq(id) = sigdtr(id) / stmp
          end if
c------------------------------------------------------------
c assign weighting to use in fit based on user chosen weight (sigwgt)
c and on data uncertainty for the space to fit in
c
          sigwgt(id) = dabs( sigwgt(id))
          if (sigwgt(id).le.zero) sigwgt(id) = one
          if (ifft(id).eq.0) then
             weight(id) = sigdtk(id) * sigdtk(id) / sigwgt(id)
          elseif (ifft(id).eq.1) then
             weight(id) = sigdtr(id) * sigdtr(id) / sigwgt(id)
          elseif (ifft(id).eq.2) then
             weight(id) = sigdtq(id) * sigdtq(id) / sigwgt(id)
          endif
 100   continue

c  do some simple error checking
       mfit = min(mfit, lenfvc)
       if (numvar.gt.mfit) then
          write(messg, '(a,i3,a,i3,a)' ) 'trying to use ', numvar,
     $         ' variables for ', mfit , ' measurements'
          call finmsg(3510,messg,' ',0)
       elseif (numvar.gt.xnidp) then
          call echo( '>WARNING:  more variables than'//
     $                 ' independent measurements in data!')
       elseif (numvar.eq.inform)  then
          call echo( '>WARNING:  equal number of variables'//
     $               ' and measurements in data!')
       end if
c
       if (iprint.ge.2) then
          write(irun,*) ' ndata, mfit, numvar=',ndata, mfit, numvar
          write(irun,*) ' xnidp              = ' , xnidp
          write(irun,*) ' -------------------------------------'
       endif
c initialization for fitting:
c  the default fitting tolerance (tolfac = 1.d-5) is empirical.
c  the user can set usrtol [default = 1]  with "toler" keyword.
       toler  = usrtol * tolfac
       chisqr = zero
       istop  = 0
c get weight for chi-square:
c     chi-square = sum{1 to mfit} [ del_chi(r) / sigma ]^2
c     but chi-square should be correctly normalized, as if the
c     sum was {1 to inform} (inform = # of independent points).
c     we already got most of this above, but now we include the
c     terms mfit and inform, so that the sum is correctly normalized
c     when we actually sum over mfit points instead of inform points
       do 280  id = 1, ndata
          weight(id) = sqrt ( nptfit(id) * weight(id) / xinfo(id))
          if (iprint.ge.2) then
             write(irun,*) ' sigdtk, sigdtr =',sigdtk(id),sigdtr(id)
             write(irun,*) ' nptfit, xinfo  =',nptfit(id),xinfo(id)
             write(irun,*) ' weight, sigwgt =',weight(id),sigwgt(id)
             write(irun,*) '---------------------------------------'
             write(irun,*) '  tolfac,usrtol = ',tolfac,usrtol
          endif
 280   continue
c
c initialize variables and fit arrays
       do 420 i =1, numvar
          xvarys(i) = xguess(i)
          xfinal(i) = xguess(i)
 420   continue
       do 440 i =1, mfit
          fvect(i) = zero
 440   continue
c
c print pre - fit message
       write(messg, '(a,f6.2,a,i3,a)' ) '        fitting ',xnidp,
     $      ' independent points with ', numvar, ' variables'
       im = istrln(messg)
       call echo(messg(:im))
c
c last chance to bail (no feff files, no data file, nofit requested)
       feff = .false.
       do 450 i = 1, mfffil
          if (feffil(i).ne.' ') feff = .true.
 450   continue
       noout = noout.or.((.not.feff).and.(.not.datafl))
       if ( nofit.or.(.not.datafl).or.
     $      (numvar.le.0).or.(.not.feff)) then
          if (nofit) then
             messg  = 'no fitting will be done, as requested.'
          elseif (.not.datafl) then
             messg = 'no fitting done. no data to fit!'
          elseif (numvar.le.0) then
             numvar = 0
             messg  = 'no fitting done. no variables defined!'
          elseif (.not.feff) then
             messg  = 'no fitting done. no feff files defined!'
          end if
          im = max(1, istrln(messg))
          call echo('        '//messg(:im))
          final = .true.
          call fitfun(mfit, numvar, xvarys, fvect, istop)
          chisqr = sumsqr(fvect, mfit)
cc          final = .false.
          go to 5000
       end if
c
c do the fitting:
cc       print*, ' FITNLS  ndata = ', ndata, ', nvarys= ', numvar
       call lmdif1 (fitfun, mfit, numvar, xvarys, fvect,
     $               toler, lminfo, iwork, work, lenwrk)
c
c print post-fit message
c   lminfo key is listed in the comments to lmdif and lmdif1
          call echo('           fitting is finished.')
       if (lminfo.eq.0)  call finmsg(3530,' ',' ',lminfo)
       if ( (lminfo.ge.4).and.(lminfo.le.7)) then
          call echo('           fit gave a warning message:')
          if (lminfo.eq.4) then
             call echo('      one or more '//
     $            'variables may not affect the fit.')
          elseif (lminfo.eq.5) then
             call echo('      too many fit '//
     $            'iterations.  try again with better')
             call echo('      better guesses or '//
     $            'a simpler problem.')
          elseif ((lminfo.eq.6).or.(lminfo.eq.7)) then
             call echo('      "toler" can probably be '//
     $            'increased without a loss of')
             write(messg, '(a,e13.5)' ) '      fit quality. '//
     $            'current value is:  toler = ', usrtol
             im = istrln(messg)
             call echo(messg(:im))
          endif
       end if
c-----------------------------------------------------------
c assign variables to final values and call fitfun one more time
       if (iprint.ge.2) then
          write(irun,*) 'fitnls before error bars: i, xfinal,dx'
          do 600 i =1, numvar
             write(irun,*) i, xfinal(i),xfinal(i) - xguess(i)
 600      continue
       endif
       iflag = 0
       final = .true.
       call fitfun(mfit, numvar, xvarys, fvect, iflag)
cc       final = .false.
c
c evaluate chi-square : note that fvect is properly
c     normalized by weight in fitfun
       nfirst = 1
       do 620 id = 1, ndata
          chi2dt(id)  = sumsqr(fvect(nfirst), nptfit(id))
          if (iprint.ge.2) then
             write(irun,*) 'nfirst, nptfit(id) =',nfirst,nptfit(id)
             write(irun,*) 'id, chi2dt(id)     =', id, chi2dt(id)
          end if
          nfirst      = nptfit(id)  + nfirst
 620   continue
       chisqr = sumsqr(fvect, mfit)
       if (iprint.ge.2) then
          write(irun,*) ' after fit : chisqr = ',  chisqr
          write(irun,*) '    i , fvect(i) '
          do 640 i = 1, mfit
             write(irun,*) i, fvect(i)
 640      continue
       endif
c
cc       print*, 'fitnls: iprint = ', iprint
c estimate the uncertainties in parameters
       call fiterr(fitfun, mfit, numvar, lenfvc, mvarys, fvect,
     $      ftemp, fjac, alpha, iprint, nerstp, xvarys,
     $      delta, correl, ier, ibadx)
c
c write final fit values to common block arrays
       do 840 i = 1, numvar
          xfinal(i) = xvarys(i)
 840   continue
       if (ier.eq.0) then
          call echo('           uncertainties estimated')
       else
          ierbar = -1
          call echo('>WARNING:  uncertainties can not'//
     $         ' be estimated.  one or more ')
          call echo('        variables did not'//
     $         ' affect the fit:')
          do 880 i = 1, numvar
             if (ibadx(i).gt.0) then
                ilen = max(1, istrln(vnames(i)))
                messg = '       >>  '//vnames(i)(1:ilen) //' <<'
                call echo(messg(:ilen+15))
             end if
 880      continue
       endif
c
 5000  continue
       if (iprint.ge.2) then
          close(irun)
          iprint = 0
       endif
       return
c
c end subroutine fitnls
       end
