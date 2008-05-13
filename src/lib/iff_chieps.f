       subroutine iff_chieps(str)
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
c given a chi(k) spectrum, estimate it's uncertainty
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'fft.h'
       save
     
       character*(*) str
       character*256  name1, chiarr, xk_arr, winarr, winnam, defkey(2)
       character      tmppre*24, cmdstr*256
       parameter (tmppre='for0internal0use0only000')
       integer  nsig1, jchi, jwin, ndfkey, jdot
       integer  istrln, k, i, ier
       integer  get_array
       logical  lpre
       double precision xkmin, xkmax, xkw, rwgt1, rwgt2, xsigr, xsigk
       double precision arr_w(maxpts), arr_c(maxpts), arr_k(maxpts)
       double precision dmarr(maxpts), xk,t,xkmax_noise
       double precision dk1, dk2, getsca, sumsqr, wtmp
       integer npts_w, npts_c, npts_k, jxar, iff_eval, iff_eval_dp
       external istrln, sumsqr, getsca, iff_eval, iff_eval_dp
       external get_array

       rwgt1  = 15.d0
       rwgt2  = 25.d0
       xkmin  = getsca('kmin',1)
       xkmax  = getsca('kmax',1)
       xkw    = getsca('kweight',1)
       dk1    = getsca('dk1',1)
       dk2    = getsca('dk2',1)
       name1  = undef
       call gettxt('kwindow', winnam)
       call gettxt('altwindow',winarr)
       call bkeys(str, mkeys, keys, values, nkeys)
       ndfkey    = 1
       defkey(1) = 'chi'
       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if ((values(i).eq.undef).and.(i.le.ndfkey)) then
             values(i) = keys(i)
             keys(i)   = defkey(i)
          end if
          if ((keys(i).eq.'prefix').or.(keys(i).eq.'group')) then 
             name1 = values(i)
             call lower(name1)
          elseif ((keys(i).eq.'rwgt1')) then
             ier = iff_eval_dp(values(i), rwgt1)
          elseif ((keys(i).eq.'rwgt2')) then
             ier = iff_eval_dp(values(i), rwgt2)
          elseif ((keys(i).eq.'kmax')) then
             ier = iff_eval_dp(values(i), xkmax)
          elseif ((keys(i).eq.'kmin')) then
             ier = iff_eval_dp(values(i), xkmin)
          elseif (keys(i).eq.'kwindow') then
             winnam = values(i)
             call lower(winnam)
          elseif (keys(i).eq.'altwindow') then
             winarr = values(i)
             call lower(winarr)
          elseif (keys(i).eq.'kweight') then
             ier = iff_eval_dp(values(i),xkw)
          elseif (keys(i).eq.'dk1') then
             ier = iff_eval_dp(values(i),dk1)
          elseif (keys(i).eq.'dk2') then
             ier = iff_eval_dp(values(i),dk2)
          elseif (keys(i).eq.'dk') then
             ier = iff_eval_dp(values(i),dk2)
             dk1 = dk2
          elseif (keys(i).eq.'chi') then
             chiarr = values(i)
             call lower(chiarr)
          elseif (keys(i).eq.'k') then
             xk_arr = values(i)
             call lower(xk_arr)
          else
             call warn(1,
     $            ' *** chi_noise: unknown key: '//keys(i)(1:k))
          end if
 100   continue 
c
c get/resolve chi(k) array name
c    check that name1 exists.  
c    otherwise, get it from chiarr, if available
c    otherwise, give up.
       if (name1.eq.undef) then
          jdot = index(xk_arr,'.')
          if (jdot.ne.0) name1 = xk_arr(1:jdot-1)
       endif
       if (name1.eq.undef) then
          jdot = index(chiarr,'.')
          if (jdot.ne.0) name1 = chiarr(1:jdot-1)
       endif
       if (name1.eq.undef) then
          call warn(2, ' chi_noise: can''t determine group name')
          return             
       endif
       call fixnam(name1,1)
       call lower(name1)

       jxar  = iff_eval(xk_arr, name1, arr_k, npts_k)
       jchi  = iff_eval(chiarr, name1, arr_c, npts_c)
       jwin  = iff_eval(winarr, name1, arr_w, npts_w)
cc       print*, ' chi_noise: ', jxar, jchi, jwin

c if the x-array has been given, the real/imag/window data should 
c be re-interpolated onto an even x-grid starting at x=0.
       if (jxar.ge.1) then
          if (jchi.ge.1)
     $         call zgrid_array(arr_k, npts_k, arr_c, npts_c, qgrid)
          if (jwin.ge.1)
     $         call zgrid_array(arr_k, npts_k, arr_w, npts_w, qgrid)
       endif

       if (jchi.le.0) then
          call warn(2, ' chi_noise: no chi(k) data array?')
          return
       end if
c
c get window function if it wasn't alreary specified
       if (jwin.le.0) then
          do 350 i = 1, maxpts
             arr_w(i) = zero
 350      continue
          call window(winnam,dk1,dk2,xkmin,xkmax,qgrid,maxpts,arr_w)
       end if
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

       do 400 i = 1, maxpts
          dmarr(i) = zero
 400   continue 

       call fitfft(arr_c, maxpts, maxfft, wfftc, qgrid,
     $      arr_w, xkw, dmarr, zero, 1, 0, rwgt1, rwgt2,
     $      nsig1,tmparr)

c  find sigma_k, the measurement uncertainty for the k-space data,
c  using the formula:
c
c                                 /      2 pi * w             \
c    (sigma_k)^2 = (sigma_r)^2 * | --------------------------  |
c                                 \ qgrid*( kmax^w - kmin^w)  /
c
c      where  w = (2 * kweight + 1).
c
       xsigr  = sqrt( sumsqr(tmparr, nsig1) / nsig1)
       wtmp   = 2 * xkw  + one
       xsigk  = xsigr * sqrt( 2 * pi * wtmp /
     $      (qgrid * (xkmax**wtmp - xkmin**wtmp )))

       call setsca('kmin',      xkmin)
       call setsca('kmax',      xkmax)
       call setsca('kweight',   xkw)
       call setsca('dk1',       dk1)
       call setsca('dk2',       dk2)
       call settxt('group',     name1)
       call setsca('epsilon_k', xsigk)
       call setsca('epsilon_r', xsigr)

cc
cc now, estimate kmax_suggest for data based on noise level.
cc 
cc this filters chi(k) using rmin=0,rmax=6, then sets kmax_suggest
cc to the largest k value for which | chi(q) | > epsilon_k
cc
cc data is stored temporarily in the group with prefix=tmppre
cc which is then erased.

       call set_array('chi', tmppre, arr_c,npts_c,1)
c fftf
       cmdstr  = 'real = '// tmppre//'.chi'
       call iff_fft('fftf',cmdstr)

c fftr       
       write(cmdstr,'(5a)')  'real = ', tmppre,
     $      '.chir_re,imag=',tmppre,'.chir_im,rmin=0,rmax=6'
       call iff_fft('fftr',cmdstr)
c get |chiq|       
       npts_c = get_array('chiq_mag',tmppre,0,arr_c)
       xkmax  = xkmin
       lpre   = .true.
       do 900 i = 1, npts_c
          xk = (i-1)*qgrid
          if (xk.ge.xkmin) then
             t = arr_c(i)/((tiny+xk)**xkw)
             if (t.ge.xsigk) then
                if (lpre) xkmax = xk
                lpre = .true.
             else
                lpre = .false.
             endif
          endif
 900   continue 
       call setsca('kmax_suggest', xkmax)

       cmdstr = '@group= '//tmppre
       call iff_erase(cmdstr)

       return
       end
c 

