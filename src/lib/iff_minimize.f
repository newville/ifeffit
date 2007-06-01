       subroutine iff_minimize(str)
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
c interface for general  function fitting
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'feffit.h'
       save
       character*(*) str
       character*32  defkey(1)
       character*256     fitarr, x_arr, errarr, name1
       double precision  xfmin, xfmax, toler, toldef
       parameter (toldef = 1.d-7)
       integer     lenwrk, lenfvc, nerstp, get_array
       parameter(lenwrk = 2*maxpts*mvarys + 20*mvarys + 2*maxpts)
       parameter(lenfvc = 2*maxpts)
       double precision   work(lenwrk), fvect(lenfvc), ftemp(lenfvc)
       double precision   fjac(lenfvc, mvarys)
       double precision   alpha(mvarys, mvarys), varys(mvarys)
       double precision   sumsqr, chired, tmp
       integer    iwork(mvarys), ibadx(mvarys), jprint, mfx
       integer    k, i, ier, ndfkey, lminfo, istrln , ilen
       integer    ixa, nxa, nofx, nfdata, imac_tmp
       logical    setx1, setx2, isamac
       integer    iff_eval_dp, iff_eval
       external   iff_eval_dp, iff_eval, sumsqr, isamac
       external   genfun, get_array, istrln, nofx
 

       restraint(1,1)   = undef
       nrestraint(1)    = 0
       fit_macro = undef
       fit_m_arg = ' '
       ifit_mac  = -1
       nerstp = 1
       jprint = 0
       toler  = zero
       usewgt = .false.
       errarr = 'fit_weight'
       x_arr  = undef
       setx1  = .false.
       setx2  = .false.
       xfmin  = zero
       xfmax  = zero

       
c  interpret any and all keyword/value pairs for setting options
       call bkeys(str, mkeys, keys, values, nkeys)
       ndfkey    =  1
       defkey(1) = 'array'
       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if ((values(i).eq.undef).and.(i.le.ndfkey)) then
             values(i) = keys(i)
             keys(i)   = defkey(i)
          end if
          if ((keys(i).eq.'array')) then
             fitarr = values(i)
          elseif ((keys(i).eq.'ordinate').or.(keys(i).eq.'x')) then
             x_arr  = values(i)
          elseif ((keys(i).eq.'uncertainty')) then
             errarr = values(i)
             usewgt = .true.
          elseif ((keys(i).eq.'xmax')) then
             ier = iff_eval_dp(values(i), xfmax)
             setx2  = .true.
          elseif ((keys(i).eq.'xmin')) then
             ier = iff_eval_dp(values(i), xfmin)
             setx1  = .true.
          elseif ((keys(i).eq.'toler')) then
             ier = iff_eval_dp(values(i), toler)
          elseif (keys(i).eq.'macro') then
             fit_macro = values(i)
          elseif ((keys(i).eq.'macro_arg')) then
             fit_m_arg = values(i)
          elseif (keys(i).eq.'restraint')  then
             if ((values(i).ne.'').and.(values(i).ne.undef)) then
                nrestraint(1)   = nrestraint(1) + 1
                restraint(nrestraint(1),1) = values(i)
             endif
          else
             call warn(1,' *** minimize: unknown key: '//keys(i)(1:k))
          end if
 100   continue
c
       call gettxt('group', name1)
       nfdata = get_array(fitarr, name1, 0, tmparr)
       if (nfdata.le.0) then
          call warn(2, ' minimize : no function to minimize?')
          return
       end if
       cfmin_pre = name1
       cfmin_arr = fitarr
       cfmin_err = ''
       iferr     = get_array(errarr, name1, 0, tmparr)
       if (iferr .gt. 0)  cfmin_err = errarr
       ifitx1 = 1
       ifitx2 = nfdata
c
c  if x array is given use it for number of fit points,
c  and look at setx1 / setx2 for user-limited fit range
       if ((x_arr.ne.undef)) then 
          ixa = iff_eval(x_arr,  name1, tmparr,nxa)
          if (ixa.ne.0) then
             if (setx1) ifitx1 = nofx(xfmin, tmparr, nxa)
             if (setx2) ifitx2 = nofx(xfmax, tmparr, nxa)
          else 
             call echo( ' minimize : cannot find ordinate function')
             call warn(1, '            for setting fit bounds')
          end if
       end if
       mfit = ifitx2 - ifitx1 + 1 + nrestraint(1)
cc       print*, 'MIN ifitx1,2,mfit = ', ifitx1, ifitx2, mfit
c
c setup and call lmdif
cc       print*, ' nvarys = ', nvarys, mfit, ifitx1, ifitx2
       if (toler .le. zero) toler = toldef
       do 200 i =1, nvarys
          varys(i) = scalar(i)
 200   continue
       do 210 i =1, mfit
          fvect(i) = zero
 210   continue
       do 220 i =1, lenwrk
          work(i) = zero
 220   continue
       do 240 i =1, mvarys
          iwork(i)  = 0
 240   continue
c
c
c determine dependencies
c note that we call iff_sync and then re-determine ifvar!!
       call iff_sync
       ifvar = get_array(fitarr, name1, 0,tmparr)
c
c  check if fit_macro call will work
       if (fit_macro.ne.undef) then
          ifit_mac = 0
          nmacxx_save =  nmacro
          nmacro = 0
          if (.not.(isamac(fit_macro,ifit_mac))) then
             tmpstr = '  fitting macro not found: '//fit_macro
             call warn(1,tmpstr)
             fit_macro = undef
             ifit_mac  = 0
          endif
c save current macro state for later
          if (ifit_mac.gt.0) then
             nmac_stop = nmacro
             imac_tmp = imac
          endif
       endif

       if (nvarys.gt.0)  then
          final = .false.
          itera = 0
          call setsca('&fit_iteration', zero)
          call chrdmp(' minimize: fitting ... ')
          call lmdif1 (genfun, mfit, nvarys, varys, fvect,
     $         toler, lminfo, iwork, work, lenwrk)

          call lm_err(lminfo,toler)
cc            if ((lminfo.ne.0).and.(lminfo.ne.4).and.(lminfo.ne.5)) then
          if (lminfo.ne.0) then 
             call chrdmp(' estimating uncertainties ... ')
             call fiterr(genfun, mfit, nvarys, lenfvc, mvarys, fvect,
     $            ftemp, fjac, alpha, jprint, nerstp, varys,
     $            delta, correl, ier, ibadx)

             if (ier.eq.0) then 
                call echo('done.')
             else 
                call echo('finished with warnings')
                call echo('  error bars not estimated. some '//
     $               'variable(s) may not affect the fit')
                call echo('  check these variables:')
                do 480 i = 1, nvarys
                   if (ibadx(i).gt.0) then
                      ilen = istrln(scanam(i))
                      call warn(1,'        '//scanam(i)(:ilen))
                   end if
 480            continue
             endif

          else
             call warn(1,' no uncertainties estimated!')
          end if
       else
          call warn(1,' minimize:  no variables defined.')
       end if
c save arrays to Program Variables
       final = .true.
       call genfun(mfit, nvarys, varys, fvect, lminfo)
       mfx    = mfit - nrestraint(1)
       chisqr = sumsqr(fvect, mfx)
       chired = chisqr / max(0.1d0, dble(mfx - nvarys))
       call setsca('chi_square',  chisqr)
       call setsca('chi_reduced', chired)
       tmp = nvarys * one
       call setsca('n_varys',     tmp)

       if (.not.usewgt) then
          do 500 i = 1, nvarys
             delta(i) = sqrt(abs(chired)) * delta(i)
             ilen = istrln(scanam(i))
             messg = 'delta_' // scanam(i)(1:ilen)
             call setsca(messg, delta(i))
 500      continue 
       endif

       if (ifit_mac.gt.0) then
          nmac_stop = 0
          imac      = imac_tmp
       endif
       return
       end







