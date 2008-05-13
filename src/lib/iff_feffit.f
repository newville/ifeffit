      subroutine iff_feffit(str)
c
c given a list of paths and data, fit sum of paths to data
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
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'fft.h'
       include 'fefdat.h'
       include 'pthpar.h'
       include 'feffit.h'
       save

       double precision  toldef, epsmin, p01
       parameter (toldef = 1.d-7, epsmin = 1.d-8, p01 = 1.d-2)
       integer   lenwrk,  lenfvc
       parameter(lenwrk = 2*maxpts*mvarys + 20*mvarys + 2*maxpts)
       parameter(lenfvc = 2*maxpts)

       logical  do_pha, do_mag, do_re, isnum, dofit, do_bkg
       character*256 str*(*), pref
       character*256 name1(mdata), nam_chi(mdata), nam_x(mdata)
       character*256 re_arr, xk_arr, winarr, winnam, fit_sp, fit_mode
       character*1024 list, cmdstr

       integer  idata, jchi, ier, illen, jk,  ilen, jwin, n1, n2
       integer  istrln, k, i, jprint, nerstp, idx, ntmp, iup
       integer  nkpts,  np, u2ipth, lminfo, nfit1, j1, j0, j2
       integer  iwork(mvarys), ibadx(mvarys), ipos, jxk, ntchi, isp
       integer  ipath_tmp(max_pathindex)
       integer  get_array, imac_tmp, j, jqw, ntbkg
       logical  iftmac, isasca, isamac, uniq_dat
       double precision  xkmin, xkmax, dk1, dk2, xkw, getsca,q, chired
       double precision  xrmin, xrmax, toler, xolow, xohigh, tmp
       double precision  work(lenwrk), fvect(lenfvc), ftemp(lenfvc)
       double precision  fjac(lenfvc, mvarys), tmpchi(maxpts)
       double precision  alpha(mvarys, mvarys), varys(mvarys), small
       double precision  qbkg1, qbkg2, bvalue,xspl(mtknot), qx
       double precision  xkdat(maxpts), xcdat(maxpts), xwdat(maxpts)
       double precision  tmparr2(maxpts)
       double precision  wtmp, stmp, sumsqr, xidat, eps_k, eps_r
       double precision  qw_tmp(mqwfs), sum2chi(mdata)
       character*128     restr_tmp(max_restraint)
       integer  nrest_tmp
       integer  iff_eval, iff_eval_dp, iff_eval_in, nqw

       external iff_eval, iff_eval_dp, iff_eval_in, bvalue
       external istrln, u2ipth, isnum,  fitfun, getsca, sumsqr
       external isasca, isamac, get_array

       toler = zero
c
       fit_macro = undef
       fit_m_arg = ' '
       ifit_mac  = -1
       nerstp= 1
       j0    = 0
       jprint= 0
       xrmin = getsca('rmin',1)
       xrmax = getsca('rmax',1)
       xkmin = getsca('kmin',1)
       xkmax = getsca('kmax',1)
       xkw   = getsca('kweight',1)
       dk1   = getsca('dk1',1)
       dk2   = getsca('dk2',1)
       xidat = getsca('data_set',1)
       idata = int(xidat)
       if (idata.le.0) idata = 1
       xidat = getsca('data_total',1)
       nfdats= int(xidat)
       eps_k = zero
       eps_r = zero
       xk_arr= undef
       winarr= undef
       pref  = undef
       call gettxt('kwindow', winnam)
       call gettxt('fit_space',fit_sp)
       call gettxt('fit_mode',fit_mode)

c  interpret any and all keyword/value pairs for setting options
       call bkeys(str, mkeys, keys, values, nkeys)
       np     =  0
       illen  =  1
       list   =  ' '
       do_re  = .false.
       do_mag = .false.
       do_pha = .false.
       do_bkg = .false.
          
       nqw = 0
       qw_tmp(1) = getsca('kweight',1)
       do 10 i = 2, mqwfs
          qw_tmp(i) = zero
 10    continue 
       nrest_tmp = 0
       
       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if ((keys(i).eq.'prefix').or.(keys(i).eq.'group')) then 
             pref  = values(i)
             call smcase(pref,'a')
          elseif (keys(i).eq.'data_set') then 
             ier   = iff_eval_dp(values(i), xidat)
             idata = int(xidat)
             name1(idata) = undef
             call setsca('data_set', xidat)
             nfit(idata) = 0
          elseif (keys(i).eq.'data_total') then 
             ier = iff_eval_dp(values(i), xidat)
             nfdats = int(xidat)
             call setsca('data_total', xidat)
          elseif (keys(i).eq.'macro') then
             fit_macro = values(i)
          elseif ((keys(i).eq.'macro_arg')) then
             fit_m_arg = values(i)
          elseif (keys(i)(1:9).eq.'restraint') then
             nrest_tmp = nrest_tmp+1
             if (idata.le.0)  idata = 0
             if (nrest_tmp.gt.max_restraint) then
                call echo(" *** feffit:: too many restraints")
                call echo("     restraint will be ignored:")
                call warn(1,"      " // values(i))
                values(i) = ''
             endif
             if((values(i).ne.'').and.(values(i).ne.undef)) then
                restr_tmp(nrest_tmp) = values(i)
cc                print*, ' Feffit restraint : ', idata
cc                print*,  nrest_tmp, values(i)(:60)
             endif
          elseif ((keys(i).eq.'rmin')) then
             ier = iff_eval_dp(values(i), xrmin)
          elseif ((keys(i).eq.'rmax')) then
             ier = iff_eval_dp(values(i), xrmax)
          elseif ((keys(i).eq.'kmax')) then
             ier = iff_eval_dp(values(i), xkmax)
          elseif ((keys(i).eq.'kmin')) then
             ier = iff_eval_dp(values(i), xkmin)
          elseif ((keys(i).eq.'error_step')) then
             ier = iff_eval_in(values(i), nerstp)
          elseif ((keys(i).eq.'error_print')) then
             ier = iff_eval_in(values(i), jprint)
          elseif (keys(i).eq.'kwindow') then
             winnam = values(i)
          elseif (keys(i).eq.'altwindow') then
             winarr = values(i)
             call lower(winarr)
          elseif (keys(i).eq.'kweight') then
             nqw = nqw + 1
             ier = iff_eval_dp(values(i), xkw)
             if ((nqw.ge.1).and.(nqw.le.mqwfs)) then
                qw_tmp(nqw) = xkw
             endif
          elseif (keys(i).eq.'dk1') then
             ier = iff_eval_dp(values(i), dk1)
          elseif (keys(i).eq.'dk2') then
             ier = iff_eval_dp(values(i), dk2)
          elseif (keys(i).eq.'dk') then
             ier = iff_eval_dp(values(i), dk1)
             dk2 = dk1
          elseif (keys(i).eq.'chi') then
             re_arr = values(i)
             call lower(re_arr)
          elseif (keys(i).eq.'k') then
             xk_arr = values(i)
             call lower(xk_arr)
          elseif ((keys(i).eq.'epsilon_k')) then
             ier = iff_eval_dp(values(i), eps_k)
          elseif ((keys(i).eq.'epsilon_r')) then
             ier = iff_eval_dp(values(i), eps_r)
          elseif ((keys(i).eq.'fit_space')) then
             fit_sp = values(i)
          elseif ((keys(i).eq.'fit_mode')) then
             fit_mode = values(i)
          elseif ((keys(i).eq.'do_real')) then
             call str2lg(values(i), do_re, ier)
          elseif ((keys(i).eq.'no_real')) then
             call str2lg(values(i), do_re, ier)
             do_re = .not. do_re
          elseif ((keys(i).eq.'do_mag')) then
             call str2lg(values(i), do_mag, ier)
          elseif ((keys(i).eq.'no_mag')) then
             call str2lg(values(i), do_mag, ier)
             do_mag = .not. do_mag
          elseif ((keys(i).eq.'do_phase')) then
             call str2lg(values(i), do_pha, ier)
          elseif ((keys(i).eq.'no_phase')) then
             call str2lg(values(i), do_pha, ier)
             do_pha = .not. do_pha
          elseif ((keys(i).eq.'do_all')) then
             call str2lg(values(i), do_re, ier)
             do_mag = do_re
             do_pha = do_re
          elseif ((keys(i).eq.'do_bkg')) then
             call str2lg(values(i), do_bkg, ier)
          elseif ((keys(i).eq.'toler')) then
             ier = iff_eval_dp(values(i), toler)
c
c path list:
          elseif (values(i).eq.undef) then
             call str2il(keys(i), max_pathindex, np, ipath_tmp,ier)
             if (ier .eq. 0) then
                jk    = istrln(keys(i))
                list  = list(1:illen)//keys(i)(1:jk)//','
                illen = illen+jk+1
             else
                call echo(' *** feffit: error generating path list')
                call warn(2, keys(i)(1:k))
             end if
          else
             call warn(1,' *** feffit: unknown key: '//keys(i)(1:k))
          end if
 100   continue 
c===================================================================
c  done reading input params
       idata  = max(1, min(mdata, idata))
       nfdats = max(1, min(mdata, nfdats))
       dofit  = (idata.ge.nfdats)
       bkgfit(idata) = do_bkg
cc       print*, ' feffit: ', idata, bkgfit(idata)

cc       print*, ' __ feffit __ : idata, nfdats = ', idata, nfdats
c===================================================================
c for this data set:
c    resolve chi(k) name, get index of chi data
c    set fft / fit params 
c    get eps_k and eps_r from iff_chieps (unless given explicitly!)
c    set path list

       name1(idata) = pref
       if (name1(idata) .eq. undef) name1(idata) = 'feffit'
       
       jchi  = get_array(re_arr, name1(idata), 0, xcdat)
       if (jchi.le.0) then
          call warn(2, ' feffit: no chi(k) data array?')
          return
       end if
c save name of re_arr for later
       nam_chi(idata) = re_arr
       nam_x(idata)   = undef
       call prenam(name1(idata),nam_chi(idata))
cc       print*, 'XXXX idata ', idata, nam_chi(idata)(1:30)
c
c if k-data is given, shift/interpolate input data onto 
c fit k-grid: chi(1) = chi(k=0).

       if (xk_arr.ne.undef) then
          ipos = 0
          jxk  = get_array(xk_arr, name1(idata), 0, xkdat)
          nam_x(idata) = xk_arr
          call prenam(name1(idata),nam_x(idata))
          if (jxk.gt.0) then
             nkpts = min(jxk, jchi)
             ntchi = int(1 + xkdat(jxk)/qgrid)
             do 130 i = 1, ntchi
                q = (i-1) * qgrid
                call lintrp(xkdat, xcdat, nkpts, q,ipos,tmpchi(i))
 130         continue
          else
             call warn(2, ' feffit: bad k-data array?')
             return
          endif
       else
          nkpts = jchi
          ntchi = jchi
          do 150 i = 1, ntchi
             tmpchi(i) =  xcdat(i)
 150      continue 
       endif
c save this chi(k) data to the appropriate array for fitfun,
c accumulate sum-of-squares for later uniqueness check
       sum2chi(idata) = zero
       do 170 i = 1, ntchi
          chiq(i,idata) = tmpchi(i)
          sum2chi(idata) = sum2chi(idata) + tmpchi(i)*tmpchi(i)
 170   continue 
       do 175 i = ntchi+1,maxpts
          chiq(i,idata) = zero
 175   continue 

c support for refining bkg(k) with EXAFS parameters
       if (bkgfit(idata)) then
          rbkg_f(idata) = xrmin
          xrmin       = zero
       endif
c
c  check if fit_macro call will work
       if (fit_macro.ne.undef) then
          ifit_mac = 0
          iftmac = isamac(fit_macro,ifit_mac)
          if (.not.iftmac) then
             cmdstr = '  fitting macro not found: '//fit_macro
             call warn(1,cmdstr)
             fit_macro = undef
             ifit_mac  = 0
          endif
          if (ifit_mac.gt.0) then
             nmac_stop = nmacro
             imac_tmp = imac
          endif
       endif
c
c  set up fft/fit parameters, initialize window functions
       small      = rgrid * p01
       n1         = int( (xrmin + small) / rgrid )  + 1
       n2         = int( (xrmax + small) / rgrid )
       if (n2.le.n1) n2 = n1 + 1
       nrpts(idata)  = n2 - n1 + 1
       rmax(idata)   = rgrid * n2
       rmin(idata)   = rgrid * n1
       dq1(idata)    = dk1
       dq2(idata)    = dk2
       small         = qgrid * p01
       n1            = int( (xkmin + small) / qgrid )  + 1
       n2            = int( (xkmax + small) / qgrid )
       if (n2.le.n1) n2 = n1 + 1
       nqpts(idata)  = n2 - n1 + 1
       qmax(idata)   = qgrid * n2
       qmin(idata)   = qgrid * n1
       nqfit(idata)  = int((qmax(idata) + 5*dq2(idata))/qgrid)

       xinfo(idata)  = 2 * ((rmax(idata)-rmin(idata))*
     $      (qmax(idata)-qmin(idata))) /pi
       if (bkgfit(idata)) then
          qbkg1       = max(zero,qgrid*n1 - dk1)
          qbkg2       = min(qgrid*n2 + dk2,  qgrid*nkpts)
          if (qbkg2 .le. (qbkg1 + 5*qgrid)) then
             qbkg2    = qbkg1 + 5 * qgrid
          endif
          ntbkg  = 1 + 2*int(rbkg_f(idata)*(qbkg2-qbkg1)/pi)
          ntbkg  = min(mtknot-korder-1,max(korder+1,ntbkg))
          nbkg(idata) = ntbkg
c-bkg where to put knots
          do 220 i = 1, korder
             qknot(i,idata)       =  qbkg1 - qgrid * (korder-i)
             qknot(ntbkg+i,idata) =  qbkg2 + qgrid * (i-1)
 220      continue
          ntmp   = ntbkg - korder + 1
          do 240 i = korder+1, ntbkg
             qknot(i, idata) = qbkg1 +(i-korder)*(qbkg2-qbkg1)/ntmp
 240      continue

c-bkg add spline coefs to the variable list, initialize to zero
          do 260  i = 1, ntbkg
             cmdstr = ' '
             write(cmdstr, 265)  'bkg',idata,'_',i,'  = 0.0'
             call iff_set('guess',cmdstr,1)
 260      continue
 265      format (a3,i2.2,a1,i2.2,a7)
          nvarys = nvarys + ntbkg
       else
          do 280  i = 1, ntbkg
 275         format (a3,i2.2,a1,i2.2)
             write(cmdstr, 275)  'bkg',idata,'_',i
             if (isasca(cmdstr)) then
                qx = getsca(cmdstr,0)
 285            format (a3,i2.2,a1,i2.2,a3,g15.9)
                write(cmdstr, 285)  'bkg',idata,'_',i,' = ',qx
                call iff_set('set', cmdstr,0)
             endif
 280      continue
       end if
c
c  which space is this fit in? 
       ifft(idata)   = 1
       if (fit_sp(1:1) .eq.'k') ifft(idata) = 0
       if (fit_sp(1:1) .eq.'q') ifft(idata) = 2

       modeft(idata)   = 1
       if (fit_mode(1:2) .eq.'ri') modeft(idata) = 0
       if (fit_mode(1:2) .eq.'rm') modeft(idata) = 1


cc       print*, ' Feffit ', fit_sp, ifft(idata), idata
c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c
c
c set k-weight, including multiple k-weights
       if (nqw.le.0) nqw = 1
       nqwfs(idata) = nqw
       do 295 i = 1, mqwfs
          qwfs(i,idata) = qw_tmp(i)
 295   continue 
       qweigh(idata) = qwfs(1,idata)

c loop over k-weights to set weight of fit
       do 450 jqw = nqw, 1, -1
          xkw = qwfs(jqw,idata)
c    determine eps_k/eps_r for this k-weight
c    call 'chieps ('chi_noise' command) to get eps_k / eps_r
          if ((eps_k .le. epsmin).and.(eps_r .le. epsmin)) then 
             ilen = istrln(nam_chi(idata))
 405         format('  chi=',a,',kmin=',f12.3,',kmax=',f12.3,
     $            ',kweight=',f12.3,',dk1=',f12.3,',dk2=',f12.3)
             write(cmdstr, 405) nam_chi(idata)(1:ilen),
     $            xkmin, xkmax,xkw,dk1,dk2
             if (nam_x(idata) .ne. undef) then
                j0 = istrln(cmdstr)
                j2 = istrln(nam_x(idata))
                cmdstr = cmdstr(:j0)//', k='//nam_x(idata)(:j2)
             endif
             call iff_chieps(cmdstr)
             sigdtk  = getsca('epsilon_k',1)
             sigdtr  = getsca('epsilon_r',1)
          elseif (eps_k .gt. epsmin) then 
             sigdtk  = eps_k
             wtmp    = 2 * xkw  + one
             sigdtr  = eps_k /  sqrt( 2 * pi * wtmp /
     $            (qgrid * (xkmax**wtmp - xkmin**wtmp )))
          elseif (eps_r .gt. epsmin) then 
             sigdtr  = eps_r
             wtmp    = 2 * xkw  + one
             sigdtk  = eps_r *  sqrt( 2 * pi * wtmp /
     $            (qgrid * (xkmax**wtmp - xkmin**wtmp )))
          end if
          call setsca('epsilon_k', sigdtk)
          call setsca('epsilon_r', sigdtr)

c set number of fit points, data-set weight, and
c a few other misc stuff based on which fit space is used
          rweigh(idata) = one
c r-space fit
          if (ifft(idata).eq.1) then
             xolow      = rmin(idata)
             xohigh     = rmax(idata)
             nfit(idata)   = 2 * max (1, nrpts(idata))
             weight(jqw,idata) = sigdtr * sigdtr 
          else
c q/k-space fit
             xolow      = qmin(idata)
             xohigh     = qmax(idata)
             nfit(idata)   = 2 * max (1, nqpts(idata))
             weight(jqw,idata) = sigdtk * sigdtk 
c q-space fit
             if (ifft(idata).eq.2) then
                wtmp       = 2 * rweigh(idata)  + one
                stmp       = sqrt ( pi * wtmp /
     $               (rgrid * (xrmax**wtmp - xrmin**wtmp )))
                sigdtq     = sigdtr / stmp
                weight(jqw,idata) = sigdtq * sigdtq 
             endif
          endif
          weight(jqw,idata) =  sqrt(nfit(idata) * weight(jqw,idata)
     $         /xinfo(idata))
          if (weight(jqw,idata).le.zero) weight(jqw,idata) = one
cc          print*, 'FIT WEIGHT: ', jqw, idata , weight(jqw,idata)
 450   continue 
c
c set k-window 
       if (winarr.eq.undef)  then
          call window(winnam,dk1,dk2,xkmin,xkmax,qgrid,maxpts,
     $         qwindo(1,idata))
       else
          jwin = get_array(winarr, name1(idata), 0,xwdat)
          if (jwin.le.0) then
             call warn(2,' feffit: no window array? looking for '
     $            //winarr)
             return
          end if
          if (xk_arr.ne.undef) then
             ipos = 0
             ntchi = int( 1 + xkdat(jxk)/qgrid )
             do 470 i = 1, ntchi
                q = (i-1) * qgrid
                call lintrp(xkdat, xwdat, maxpts,
     $               q, ipos, qwindo(i,idata))
 470         continue
          else
             ntchi = jwin
             do 480 i = 1, ntchi
                qwindo(i,idata) =  xwdat(i)
 480         continue 
          endif
       end if
c set r-window 
       call window('hann',zero,zero,xrmin,xrmax,rgrid,maxpts,
     $         rwindo(1,idata))

c convert list of path indices to iup array
       call str2il(list(1:illen), max_pathindex, np,
     $      ipath_tmp, ier)

       do i = 1, mpaths
          iulist(i,idata) = 0
       enddo
       iup= 0
       do i = 1, np
          if (u2ipth(ipath_tmp(i)).ge.1) then
             iup=iup+1
             iulist(iup,idata) = ipath_tmp(i)
          endif
       enddo
cc       print *, 'iulist ', idata, iup, np
cc       do i = 1, iup
cc          print*, iulist(i,idata)
cc       enddo
cc       print*,' idata ', idata, nrest_tmp
       nrestraint(idata) = nrest_tmp
       do 570 i = 1, nrest_tmp
          restraint(i,idata) = restr_tmp(i)
 570   continue 
cc       print*,' idata / nrestraint ', idata, idx, nrest_tmp

c===================================================================
c if not doing fit (multiple data-set fit), bail out now:
       if (.not.dofit)  then
          call echo('  feffit: not fitting until all data '//
     $         'sets defined')
          return
       end if

c everything below assumes we're really, really doing a fit
c
c read the needed feff arrays
       call fefinp
c


c   set up fit arrays
c   sync variables
c   check # of variables  and # fit points
c   call lmdif1 and fiterr
       write(cmdstr, '(1x,a,i3,a)')  ' feffit fitting ',
     $         nfdats, ' data sets'
       call echo(cmdstr)
       mfit = 0

       
       do 580 idx = 1, nfdats
          mfit = mfit + (nfit(idx) + nrestraint(idx))* nqwfs(idx)
 580   continue 
       mfit = min(mfit, lenfvc)
cc       print*, '  Feffit ', mfit, idx, nfit(idx), nqwfs(idx), nfdats,
cc     $      nrestraint(idx)
cc       if (nrestraint(idx) .ge.1) then
cc          print*, '  Feffit using restraints:',nrestraint(idx)
cc       endif
c===================================================================
c  set up arrays for non-linear fit with lmdif / fitfun
c===================================================================
c
       if (nfit(idata).le.4) then
          call echo('   feffit: too few fit data points: ')
          if (ifft(idata).eq.1) then
             write(cmdstr, 625)  '  Fit R range = [',xolow,
     $            ' : ', xohigh, ']'
          else
             write(cmdstr, 625)  '  Fit k range = [',xolow,
     $            ' : ', xohigh, ']'
          endif
 625      format (a,f8.3,a,f8.3,a)
          call warn(2,cmdstr)
          return
       endif
       if (nvarys.gt.nfit(idata)) then
          call warn(1,' *** feffit: fewer fit data points '//
     $         'than variables! fit may not work.')
       endif


c synchronize the math expressions, set up fit variables
       call iff_sync
       if (toler .le. zero) toler = toldef
       do 700 i =1, nvarys
          varys(i) = scalar(i)
 700   continue
       do 710 i =1, mfit
          fvect(i) = zero
 710   continue
       do 720 i =1, lenwrk
          work(i) = zero
 720   continue
       do 740 i =1, mvarys
          iwork(i)  = 0
 740   continue
c      
c  the real fit
       if (nvarys.gt.0)  then
          final = .false.
          itera = 0
          call setsca('&fit_iteration', zero)
          call chrdmp('  fitting ... ')

          call fitfun(mfit, nvarys, varys, fvect, lminfo)

          call lmdif1 (fitfun, mfit, nvarys, varys, fvect,
     $         toler, lminfo, iwork, work, lenwrk)

          call lm_err(lminfo,toler)
cc          if ((lminfo.ne.0).and.(lminfo.ne.4).and.(lminfo.ne.5)) then
          if (lminfo.ne.0) then
             call chrdmp('  estimating uncertainties ... ')
             call fiterr(fitfun, mfit, nvarys, lenfvc, mvarys, fvect,
     $            ftemp, fjac, alpha, jprint, nerstp, varys,
     $            delta, correl, ier, ibadx)
             if (ier.eq.0) then 
                call echo('done.')
             else 
                call echo('finished with warnings')
                call echo('  error bars not estimated. some '//
     $               'variable(s) may not')
                call echo('  affect the fit. Check these variables:')
                do 880 i = 1, nvarys
                   if (ibadx(i).gt.0) then
                      ilen = istrln(scanam(i))
                      call echo('        '//scanam(i)(:ilen))
                   end if
 880            continue
                call set_status(1)
             endif
          else
             call warn(1,' no uncertainties estimated!')
          end if
       else
          call warn(1,' feffit:  no variables defined.')
       endif
c 
c save scalars to Program Variables
       final = .true.
       call synvar
       call fitfun(mfit, nvarys, varys, fvect, lminfo)
       chisqr = sumsqr(fvect, mfit)

c add total number of independent points
c  avoid double counting by inspecting chi(k) names and sum-of-(chi*chi):
c  sure, this can be fooled by adding 1.e-8 to chi(k=0), but....
       xnidp  = xinfo(1)
       do 920 i = 2, nfdats
          uniq_dat  = .true.
          do 910 j = 1, i-1
             if ( (nam_chi(i).eq.nam_chi(j) .or.
     $            (sum2chi(i).eq.sum2chi(j))))
     $            uniq_dat = .false.
 910      continue 
          if (uniq_dat)  xnidp = xnidp + xinfo(i)
 920   continue 

       chired = chisqr / max(0.1d0, (xnidp - nvarys))
       call setsca('chi_square',  chisqr)
       call setsca('chi_reduced', chired)
       call setsca('r_factor',    rfact_total)
       call setsca('n_idp', xnidp)

       call setsca('kweight', qwfs(1,idata))
       call setsca('kmin',   xkmin)
       call setsca('kmax',   xkmax)
       call setsca('dk1',    dk1)
       call setsca('dk2',    dk2)
       call setsca('rmin',   xrmin)
       call setsca('rmax',   xrmax)

       tmp = nvarys * one
       call setsca('n_varys', tmp)
      
       do 960 i = 1, nvarys
          delta(i) = sqrt(abs(chired)) * delta(i)
          ilen = istrln(scanam(i))
          messg = 'delta_' // scanam(i)(1:ilen)
          call setsca(messg, delta(i))
 960  continue 
c
c  fit with a macro is done, so reset stopping point
c  and previous macro pointer 
       if (ifit_mac.gt.0) then
          nmac_stop = 0
          imac      = imac_tmp
       endif
c

c save arrays to Program Variables for each data set
c
       do 3000 idata = 1, nfdats
c          print*, 'FEFFIT ', idata, ' | ' ,
c     $         name1(idata)(1:20), ' | ', nam_chi(idata)(1:20) 

c NOTE:
c  thiq(i,data) holds the fit residual, so we'll need to 
c  add back the data chi(k) here, and possibly the bkg(k) 

c--k
cc        call set_array('resid', name1(idata), thiq(1,idata), nkpts,1)
          do 2200 i = 1, nkpts
             tmparr(i)   = qgrid * (i - 1)
             thiq(i,idata)  = thiq(i,idata)+chiq(i,idata)
 2200     continue 
          call set_array('k',   name1(idata), tmparr, nkpts, 1)
          call set_array('chi', name1(idata), thiq(1,idata), nkpts,1)
c 
c--background chi(k)
          if (bkgfit(idata)) then
             do 2310 isp = 1, nbkg(idata)
                write(cmdstr, 2325)  'bkg',idata,'_',isp
                xspl(isp) = getsca(cmdstr,0)
 2310        continue
 2325        format (a,i2.2,a,i2.2)
             do 2330 i = 1, nkpts
                qx = qgrid*(i-1)
                tmparr(i) =  bvalue(qknot(1,idata), 
     $               xspl,nbkg(idata),korder,qx,0)
                thiq(i, idata) = thiq(i, idata) - tmparr(i)
 2330        continue 
             call set_array('kbkg', name1(idata), tmparr, nkpts, 1)
          endif 
ccc--chi , which does NOT include the background!
ccc      call set_array('chi', name1(idata), thiq(1,idata), nkpts,1)

c--chi_real
          if (do_re) then
             call set_array('chi_real', name1(idata),
     $            thiqr(1,idata), nkpts, 1)
          end if
c--chi_mag
          if (do_mag) then
             do 2240 i = 1, nkpts
                tmparr(i) = sqrt(
     $               thiqr(i,idata)**2 + thiq(i,idata)**2 )
 2240        continue 
             call set_array('chi_mag', name1(idata), tmparr,nkpts,1)
          end if
c--chi_phase
          if (do_pha) then
             do 2280 i = 1, nkpts
                tmparr(i) = atan2( thiq(i,idata), thiqr(i,idata))
                if (i.gt.1)  call pijump(tmparr(i), tmparr(i-1))
 2280        continue 
             call set_array('chi_phase', name1(idata), tmparr,nkpts,1)
          end if
c      
c      construct r-space data of data and fit
c-data
cc          print*, ' NEAR done: nam_chi = ', nam_chi(idata)(1:40)
cc          print*,  ' name1: ', name1(idata)(1:40)
          j1 = istrln(nam_chi(idata))
          cmdstr  = 'real='// nam_chi(idata)(1:j1)
          if (nam_x(idata) .ne. undef) then
             j0 = istrln(cmdstr)
             j2 = istrln(nam_x(idata))
             cmdstr = cmdstr(:j0)//', k='//nam_x(idata)(:j2)
          endif
          call iff_fft('fftf',cmdstr)
c-fit
          nam_chi(idata) = 'chi'
          call prenam(name1(idata),nam_chi(idata))
          j1 = istrln(nam_chi(idata))
          cmdstr  = 'real = '// nam_chi(idata)(1:j1)
          call iff_fft('fftf',cmdstr)
 3000  continue 

c  guess what?   we're done!
       return
       end

