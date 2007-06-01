      subroutine fitlog
c
c      write log file to summarize fitting run for feffit.
c      the layout here is hoped to be user-friendly.
c      feel free to change any of this at any time.
c
c      copyright 1993 university of washington         matt newville
c
c----------------------------------------------------------------------
       include 'fitcom.h'

       character*40     logfil, stat*7, outstr*128
       double precision par(mdpths, mpthpr), rfmin, rfave
       double precision redchi, sqrchi, order, small, ten
       double precision tmp1(mvarys**2), tmp2(mvarys**2)
       double precision decod, reff,corr, xnu
       integer      ilog, i, istrln, nstart,id,ilo,inpath
       integer idpath,im,ipath,juser,jfeff,ind, nvr1, nvr2
       integer jlen, ntmp, n, nv, ilen, iex, ierr, idot
       parameter ( rfmin =0.05d0, small = 1.d-5, ten = 10.d0)
       external decod, istrln
c----------------------------------------------------------------------
c       winstr(1) =  'hanning window sills'
c       winstr(2) =  'hanning window fraction'
c       winstr(3) =  'gaussian window'
c       winstr(4) =  'lorentzian window'
c       winstr(5) =  'parzen window '
c       winstr(6) =  'welch window '
c       winstr(7) =  'sine window '
c       winstr(8) =  'gaussian window (2nd form)'

       stat      = 'unknown'
       if (vaxflg)  stat = 'new'
c  determine log file name (usually trivial)
c  but if not, find the *last* "."
       logfil = 'feffit.log'
       if (inpfil.ne.'feffit.inp') then
          call triml(inpfil)
          idot  = max(1,istrln(inpfil))
          if (index(inpfil(1:idot),'.').ne.0) then
 15          continue
             if (index(inpfil(idot:idot),'.').eq.0) then
                idot = idot - 1
                go to 15
             end if
          end if
          logfil = inpfil(1:idot) //'log'
       end if
c  open log file
       ilog   = 0
       ierr   = 0
       iex    = 0
       call openfl(ilog, logfil, stat, iex, ierr)
       if (ierr.lt.0) call finmsg(1002,' ',logfil,0)
c
c  write preliminary messages:
       ilen   = max(1, istrln(versn))
       write(ilog,9002) '   '//versn(1:ilen)
       write(ilog,9904)
c  list input data sets, titles for each
c  also, compute average r-factor
       if (ndata.eq.0) then
          write(ilog,9003) 'no input data files.'
       else
          if (ndata.eq.1) then
             write(ilog,9003) 'input data file:'
          else
             write(ilog,9003) 'input data files:'
          endif
          rfave  = zero
          do 80 n = 1, ndata
             rfave = rfave + rfactr(n)
             if (ndata.gt.1)  write(ilog,9055) '-> data set ',n
             if (datain(n)) then
                ilen = max(1, istrln(chifil(n)))
                outstr = 'input data chi file = '
     $               //chifil(n)(:ilen)//', skey = '//skey(n)
             else
                outstr = ' no input data file given'
             endif
             ilen = max(1,istrln(outstr))
             write(ilog,9005) outstr(1:ilen)
c##<bkg
             if (bkgdat(n)) then
                ilen = max(1, istrln(bkgfil(n)))
                outstr = 'used bkg(k) from file = '
     $               //bkgfil(n)(:ilen)//', skey = '//skeyb(n)
                ilen = max(1,istrln(outstr))
                write(ilog,9005) outstr(1:ilen)
             end if
             if (bkgfit(n)) then
                write(ilog,9005) '  fitted background spline to data'
                write(ilog,9015) '  with rbkg = ', rbkg(n),
     $               '  and n_knots = ',nbkg(n)
             end if
c##bkg>
 80       continue
       end if
       write(ilog,9902)
c----------------------------------------------------------------
c  preliminary : list data sets, weight for each
c     full fit : total indep. points, fit parameters, chi-square, reduced
c     full fit : variables, uncertainties, correlations, init. guesses
c     full fit : user-defined functions
c
       xnu    = xnidp - numvar
       outstr = 'fit results, goodness of fit, and error analysis:'
       ilen = max(1,istrln(outstr))
       write(ilog,9003) outstr(1:ilen)
       if (dabs(tranq-2).gt.small) then
          write(ilog,9910) 'used tranquada correction = ', tranq
       end if
       if (rm2flg) write(ilog,9903) 'used {reff}^{-2} in XAFS Eqn'
       if (ndata.gt.1)
     $      write(ilog,9055) 'number of data sets           = ',ndata
       write(ilog,9060) 'independent points in data    = ',xnidp
       write(ilog,9055) 'number of variables in fit    = ',numvar
       write(ilog,9060) 'degrees of freedom in fit     = ',xnu
       rfave = rfave / ndata
       write(ilog,9035) 'r-factor of fit               = ',rfave
       if (rfave.gt.rfmin)  write(ilog,9903)
     $     ' warning: this r-factor is too big.  the fit might be bad.'
       if (chisqr.lt. 999 999) then
          write(ilog,9035) 'chi-square                    = ',chisqr
       else
          write(ilog,9036) 'chi-square                    = ',chisqr
       end if
       redchi =  chisqr
       if (xnu.gt.0) then
          redchi =  chisqr / xnu
          if (redchi.lt. 999 999) then
           write(ilog,9035) 'reduced chi-square            = ',redchi
          else
           write(ilog,9036) 'reduced chi-square            = ',redchi
          end if
       elseif (xnu.eq.0) then
          write(ilog,9005) 'questionable fit: all independent ',
     $         'points were used in fit!'
       else
          write(ilog,9005) 'invalid fit: more variables than',
     $         ' independent points !'
       end if
       write(ilog,9003) ' '
c
c---------------------------------------------------------------
c  write out the variables and uncertainties, and correlations
c
       if (numvar.le.0) then
          write(ilog,9005) 'no variables were used in feffit'
       else
          outstr = 'feffit found the following values for '//
     $             'the variables:'
          ilen = max(1,istrln(outstr))
          write(ilog,9005) outstr(1:ilen)
          outstr = 'variable            best fit value    '//
     $             'uncertainty  initial guess'
          ilen = max(1,istrln(outstr))
          write(ilog,9002) '      '//outstr(1:ilen)
c
c    go through all values, deciding which were variables, and
c    if any "set" values (user-defined functions) were used.
          sqrchi =   sqrt(dabs(redchi))
          do 140 i = 1, numvar
             ilen = max(15,  istrln(vnames(i)))
             write(ilog,9070) vnames(i)(1:ilen),'=', xfinal(i),
     $            sqrchi * delta(i), xguess(i)
 140      continue
          write(ilog,9003) '    '
c    if successfully calculated, report error bars,
c    otherwise report which variables might be the problem ones
          if (ierbar.lt.0) then
             write(ilog,9002)
     $     '       uncertainties could not be estimated. at least'
             write(ilog,9002)
     $     '       one of the variables does not affect the fit.'
             do 180 nv = 1, numvar
                   ilen = max(25, istrln(vnames(nv) ))
                   if (dabs(xfinal(nv) - xguess(nv)).le.small) then
                      write(ilog,9007)  '      check  '
     $                     //vnames(nv)(1:ilen)
                   end if
 180         continue
          else
c
c write out correlation matrix, sorted so that most highly correlated
c pairs of variables are printed first.
c       first load array with abs(correl) and with j*numvar + i
             write(ilog,9005) 'correlation between variables '
             outstr = 'variable #1    variable #2         correlation'
             ilen = max(1,istrln(outstr))
             write(ilog,9007) outstr(1:ilen)
             ntmp = 0
             if (numvar.eq.2) then
                 ilen = max(15, istrln(vnames(1)))
                 jlen = max(15, istrln(vnames(2)))
                 write(ilog,9080) vnames(1)(:ilen),
     $                            vnames(2)(:jlen), correl(1,2)
             elseif (numvar.gt.2) then
                do 230 nvr1 = 1, numvar
                   do 220 nvr2 = nvr1+1, numvar
                      ntmp       = ntmp + 1
                      tmp1(ntmp) = dabs(correl(nvr1, nvr2))
                      tmp2(ntmp) = nvr2 * numvar + nvr1
 220               continue
 230            continue
                call sort2(ntmp,tmp1,tmp2)
                do 270 n  = ntmp, 1, -1
                   ind = int(tmp2(n))
                   if (ind.gt.0) then
                      nvr1 = mod(ind, numvar)
                      nvr2 = ind / numvar
                      corr = dabs( correl(nvr1,nvr2) )
                      if (corr.ge.cormin) then
                         ilen = max(15, istrln(vnames(nvr1)))
                         jlen = max(15, istrln(vnames(nvr2)))
                         write(ilog,9080) vnames(nvr1)(:ilen),
     $                     vnames(nvr2)(:jlen), correl(nvr1,nvr2)
                      else
                         outstr='all other correlations are less than'
                         ilen   = max(1, istrln(outstr))
                         write(ilog,9030) outstr(1:ilen+1), cormin
                         go to 275
                      endif
                   endif
 270            continue
 275            continue

c
             end if
cc             write(ilog,9903)
cc     $'--------------------------------------------------------------'
cc             write(ilog,9903)
cc     $'the uncertainties and correlations listed above are estimated '
cc             write(ilog,9903)
cc     $'under the assumption that the errors are normally distributed.'
cc             write(ilog,9903)
cc     $'                                                              '
cc             write(ilog,9903)
cc     $'the uncertainties given are estimated to increase the best-fit'
cc             write(ilog,9903)
cc     $'value of chi-square by the value of reduced chi-square.       '
cc             write(ilog,9903)
cc     $'     this assumes the fit is "good" and that the value of     '
cc             write(ilog,9903)
cc     $'     the measurement uncertainty was poorly estimated.        '
cc             write(ilog,9903)
cc     $'--------------------------------------------------------------'
          end if
       end if
c
c--------------------------------------------------------------
c  for each data set :
c     titles, fft, indep. points, measure uncertainty, chi-square
c     feff paths, with path parameters
c
       id = 1
 500   continue
       if (ndata.gt.1)  then
         write(ilog,9904)
         write(ilog,9055) 'data set ',id
         write(ilog,9003) 'measurements for this data set only:'
       end if
c data file name
       if (datain(id)) then
          ilen = max(1, istrln(chifil(id)))
          outstr = 'input data chi file = '
     $             //chifil(id)(:ilen)//', skey = '//skey(id)
          write(ilog,9005) 'user titles:'
          do 540 i = 1, mtitle
             call triml(titles(i,id))
             ilen = istrln(titles(i,id))
             if (ilen.gt.0) write(ilog,9007) titles(i,id)(:ilen)
 540      continue
c
       else
          outstr = ' no input data used'
       endif
       ilo = max(1,istrln(outstr))
       write(ilog,9005) outstr(1:ilo)
c
c information content and uncertainty information for this data
       write(ilog,9005) 'measurement uncertainty of data:'
       write(ilog,9035) '             ... in k-space     = ',sigdtk(id)
       write(ilog,9035) '             ... in r-space     = ',sigdtr(id)
       write(ilog,9035) '             ... in q-space     = ',sigdtq(id)
       write(ilog,9035) 'user-chosen weight for data     = ',sigwgt(id)
       write(ilog,9035) 'weight used for chi-square      = ',weight(id)
       write(ilog,9035) 'independent points in data      = ',xinfo(id)
       if (ndata.gt.1) then
          write(ilog,9035) 'partial chi-square for data set = ',
     $         chi2dt(id)
          write(ilog,9035) 'r-factor for data set           = ',
     $         rfactr(id)
       else
          write(ilog,9035) 'chi-square                      = ',
     $         chi2dt(id)
          write(ilog,9035) 'r-factor                        = ',
     $         rfactr(id)
       end if
c fitting range, fft details
       if (ifft(id).eq.0) then
          write(ilog,9005) 'fitting was done in original k-space'
          write(ilog,9902)
          write(ilog,9020) 'k range    = [',qmin(id),',',qmax(id),' ]'
       elseif (ifft(id).eq.1) then
          write(ilog,9005) 'fitting was done in r-space'
          write(ilog,9020) 'r range    = [',rmin(id),',',rmax(id),' ]'
          write(ilog,9902)
          write(ilog,9003) 'fourier transform information:'
          write(ilog,9020) 'k range    = [',qmin(id),',',qmax(id),' ]'
          write(ilog,9030) 'k weight   =  ', qweigh(id)
          write(ilog,9020) 'dk1, dk2   =  ',qwin1(id),',',qwin2(id),' '
          write(ilog,9005) 'window type= '//sqwin(id)
       elseif (ifft(id).eq.2) then
          write(ilog,9005) 'fitting was done in '//
     $         'backtransformed k-space'
          write(ilog,9020) 'k range    = [',qmin(id),',',qmax(id),' ]'
          write(ilog,9902)
          write(ilog,9003) 'fourier transform information (k->r):'
          write(ilog,9020) 'k range    = [',qmin(id),',',qmax(id),' ]'
          write(ilog,9030) 'k weight   =  ', qweigh(id)
          write(ilog,9020) 'dk1, dk2   =  ',qwin1(id),',',qwin2(id),' '
          write(ilog,9005) 'window type= '//sqwin(id)
          write(ilog,9003) 'fourier transform information (r->k):'
          write(ilog,9020) 'r range    = [',rmin(id),',',rmax(id),' ]'
          write(ilog,9030) 'r weight   =  ', rweigh(id)
          write(ilog,9020) 'dr1, dr2   =  ',rwin1(id),',',rwin2(id),' '
          write(ilog,9005) 'window type= '//srwin(id)
       endif
       write(ilog,9055) 'number of points in fft for fitting = ',mftfit
       write(ilog,9055) 'number of points in fft for outputs = ',mftfit
       write(ilog,9902)
c--------------------------------------------------------------
c  write out the "set" values of user defined functions
       if (nmathx.eq.numvar) then
          outstr = 'no "set" values were used in feffit'
          write(ilog,9005) outstr
       else
          outstr = 'feffit found the following values '//
     $             'for the "fixed" values:'
          ilo = max(1,istrln(outstr))
          write(ilog,9005) outstr(1:ilo)
c     ----------------------------------------
c     first evaluate the set values for this data set
          inpath    = max(1, jdtpth(1,id))
          jfeff     = max(1, jpthff(inpath))
          reff      = max(small, refpth(jfeff))
          ixpath    = jfeff
          consts(4) = reff
          call setval(numvar+1,nmathx,icdval,maxval,micode,
     $          consts,mconst,values,icdloc,mlocal,mdata,ixlocl,id)
          do 700 i = numvar+1, nmathx
             ilen      = max(20, istrln(vnames(i)))
             order     = dabs(log(dabs( values(i)) + small ))
             if (order.lt.ten) then
                write(outstr,9310) vnames(i)(1:ilen), values(i)
             else
                write(outstr,9320) vnames(i)(1:ilen), values(i)
             end if
             if (icdval(1,i).gt.ixlocl)
     $            call append(outstr,'          (local) ',im)
             ilo = max(1,istrln(outstr))
             write(ilog,9005) outstr(1:ilo)
 700      continue
          write(ilog,9902)
       end if
c     ----------------------------------------
c     for each path, write out the index and label, and
c     calculate and save the numerical path parameters.
c
c write out names of feff.dat files
       write(ilog,9003) 'path   feff file name'
c
c    iuser    "user path index" written in feffit.inp
c    inpath   "internal path index"  for the path parameters [mpaths]
c    ifeff    "feff path index"  for the feff files          [mfffil]
c    idpath   "data path index"  for each data set  [0:mdpths, mdata]
c    pointers held in common:
c            jdtpth(idpath,idata) = inpath
c            jdtusr(idpath,idata) = iuser
c            jpthff(inpath)       = ifeff
       do 1500 ipath = 1, mdpths
          inpath = jdtpth(ipath,id)
          if (inpath.gt.0) then
             jfeff  = jpthff(inpath)
             ixpath = jfeff
             juser  = jdtusr(ipath, id)
             call triml(feffil(jfeff))
             ilen = max(1, istrln(feffil(jfeff)))
             if (iffrec(jfeff).eq.0) then
                write(ilog,9100) juser, feffil(jfeff)(1:ilen)
             else
                write(ilog,9105) juser, feffil(jfeff)(1:ilen),
     $               iffrec(jfeff)
             endif
          end if
 1500  continue
c write out user id's
       write(ilog,9902)
       write(ilog,9003) 'path   identification '
       do 1600 ipath = 1, mdpths
          inpath = jdtpth(ipath,id)
          if (inpath.gt.0) then
             jfeff  = jpthff(inpath)
             juser  = jdtusr(ipath, id)
             call triml(pthlab(inpath))
             ilen = min(70, istrln(pthlab(inpath)) )
             if (ilen.le.0) then
                ilen = min(70, max(1, istrln(feffil(jfeff))))
                if (iffrec(jfeff).eq.0) then
                   write(pthlab(inpath),9450)
     $                  feffil(jfeff)(1:ilen), refpth(jfeff),
     $                  degpth(jfeff), nlgpth(jfeff)
                else
                   write(pthlab(inpath),9455)
     $                  feffil(jfeff)(1:ilen), iffrec(jfeff),
     $     refpth(jfeff), degpth(jfeff), nlgpth(jfeff)
                end if
             endif
             ilen = min(70, max(1, istrln(pthlab(inpath))))
             write(ilog,9100) juser, pthlab(inpath)(1:ilen)
          end if
 1600  continue
c--------------------------------------------------------------
c path parameters
c  evaluate the path parameters from the values and defaults
       do 2000 idpath = 1, mdpths
          inpath    = jdtpth(idpath,id)
          if (inpath.eq.0)    go to 1990
          jfeff     = jpthff(inpath)
          reff      = refpth(jfeff)
          ixpath    = jfeff
          consts(4) = reff
c     evaluate the non-variable values
c  evaluate the non-variable values
c     in case they depend on reff, local values to data set, etc
c##          if (numvar.gt.0) then
             nstart = nconst + numvar + 1
             call setval(nstart,nmathx,icdval,maxval,micode,
     $          consts,mconst,values,icdloc,mlocal,mdata,ixlocl,id)
c##          end if
c     evaluate the path parameters from the values and defaults
c     (these were found in fitfun before this routine was called)
          do 1950 i = 1, mpthpr
             par(idpath,i) = decod(icdpar(1, i, inpath), micode,
     $                             consts, values, defalt(i))
 1950     continue
c
 1990     continue
 2000  continue
c     write out first set of numerical path parameters
       write(ilog,9902)
       outstr = 'path    degen     amp        e0'//
     $          '    {reff + delr}   delr        sigma2'
       ilo = max(1,istrln(outstr))
       write(ilog,9003) outstr(1:ilo)
       do 3500 idpath = 1, mdpths
          inpath = jdtpth(idpath,id)
          if (inpath.gt.0) then
             jfeff     = jpthff(inpath)
             write(ilog,9150) jdtusr(idpath, id), degpth(jfeff),
     $            par(idpath, jps02), par(idpath, jpe0),
     $            refpth(jfeff) + par(idpath, jpdelr),
     $            par(idpath, jpdelr), par(idpath, jpsig2)
          end if
 3500  continue
c
c   e_imag, third and fourth cumulants, phase shift
       write(ilog,9902)
       outstr = 'path      ei        third      fourth'
       if (dphflg) call append(outstr,'     dphase',im)
       ilo = istrln(outstr)
       write(ilog,9003) outstr(1:ilo)
c
       do 3600 idpath = 1, mdpths
          inpath = jdtpth(idpath,id)
          if (inpath.gt.0) then
             jfeff  = jpthff(inpath)
             if (.not.dphflg) then
                write (ilog,9253) jdtusr(idpath, id),
     $               par(idpath, jpei ), par(idpath, jp3rd),
     $               par(idpath, jp4th)
             else
                write (ilog,9254) jdtusr(idpath, id),
     $               par(idpath, jpei ), par(idpath, jp3rd),
     $               par(idpath, jp4th), par(idpath, jpdpha)
             end if
          end if
 3600  continue
c
c  continue to next data set
c
       id = id + 1
       if (id.le.ndata) go to 500
c   finished
       write(ilog,9902)
       close(unit=ilog)
       return
c
c     formats
 9002  format(2x,a)
 9003  format(3x,a)
 9005  format(5x,a)
 9007  format(7x,a)
 9015  format(5x, a, f6.3, 1x, a, i3)
 9020  format(5x,2(a,f10.5),a)
 9030  format(5x,a,f10.5)
 9035  format(5x,a,f14.6)
 9036  format(5x,a,e14.6)
 9055  format(5x,a,2x,i5)
 9060  format(5x,a,f11.3)
 9070  format(7x,2a,3(2x,f14.6))
 9080  format(7x,2a,2x,f14.6)
 9100  format(2x,i5,3x,a)
 9105  format(2x,i5,3x,a,1x,',',1x,i5)
 9150  format(4x,i3,3x,f6.2,3x,f7.4,2x,f9.5,3x,f8.4,2x,f9.5,3x,f10.6)
 9253  format(4x,i5,3(1x,f10.6))
 9254  format(4x,i5,4(1x,f10.6))
 9255  format(4x,i5,5(1x,f10.6))
 9256  format(4x,i5,6(1x,f10.6))
 9310  format(2x,a,' = ',f14.6)
 9320  format(2x,a,' = ',e14.6)
 9450  format(a,': r=',f6.3,'; n=',f6.2,'; nlegs=',i3)
 9455  format(a,' ,',i5,': r=',f6.3,'; n=',f6.2,'; nlegs=',i3)
 9903  format(3x,'>> ',a,' <<')
 9910  format(3x,'>> ',a,f9.4,' <<')
 9902  format(2x,70('-'))
 9904  format(2x,70('='))
c
c end subroutine fitlog
       end
