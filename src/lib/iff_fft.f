       subroutine iff_fft(key,str)
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
c purpose: ifeffit: xafs fourier transform -- driver for xafsft
c
c arguments:
c      key     command name to perform (fftf / fftr)    [in]
c      str     command line to performd                 [in]
c
c notes:
c   1. driver for xafsft, does both forward and back transforms
c   2. key names direction of fft
c   3. uses named scalars, strings, and arrays, as well as
c      key/value pairs from command string
c
c requires: istrln, getsca, gettxt, bkeys, lower, str2re,
c           echo, cffti, window, lintrp, xafsft, setsca, settxt
c
c   Despina can be reached in two ways: by ship or by camel.  The city
c   displays one face to the travel arriving overland and a different
c   one to him who arrives by sea.
c
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'fft.h'
       include 'pthpar.h'
       include 'fefdat.h'
       save
       integer  nxpar, ifft
       parameter (nxpar = 11)
       character*(*) key, str, defkey(3)*64, winnam*32
       character*512 re_arr, im_arr, x_arr, pc_arr, tnam
       character*512 winarr, name1, wintyp, outtyp, myname*5
       character*10  forpar(nxpar), revpar(nxpar), params(nxpar)
       complex*16    cinp(maxfft), cout(maxfft), cmi
       parameter (cmi=(0d0,-1d0))

       double precision getsca, xmin, xmax, xw, dx1, dx2, xmout
       double precision xgrid, ygrid,xk
       double precision arr_x(maxpts), arr_r(maxpts), arr_i(maxpts)
       double precision arr_w(maxpts), arr_p(maxpts)
       double precision txx(maxpts), txr(maxpts), txi(maxpts)
       double precision txm(maxpts), txp(maxpts)
       integer  npts_x, npts_r, npts_i, npts_w, npts_p, jxar,nptsxx
       integer  jwin, jrea, jima, jpha, i, k, jdot, idwin
       integer  jout1, jout2, jout3, jout4, jout5
       integer  ier,  istrln, ndfkey, ipos, nyout, nxout
       integer  pc_path, jfeff, ilen
       integer  iff_eval, iff_eval_dp, iff_eval_in, u2ipth
       logical  xm_set , pc_caps
       character*16 pc_edge, path*512
       external iff_eval, iff_eval_dp, iff_eval_in
       external getsca, istrln, u2ipth

       data (forpar(i), i=1, nxpar) /'k', 'dk', 'dk1', 'dk2',
     $      'kmin', 'kmax', 'kweight', 'kwindow', 'win', 'rsp',
     $      'rmax_out'/

       data (revpar(i), i=1, nxpar) /'r', 'dr', 'dr1', 'dr2',
     $      'rmin', 'rmax', 'rweight', 'rwindow', 'rwin', 'qsp',
     $      'qmax_out'/
c
c initialize 
c set default program variables to use based on "key"
       call iff_sync
       if ( (key.eq.'fftr').or.(key.eq.'bft')) then
          ifft   = -1
          xgrid  = rgrid
          ygrid  = qgrid
          do 12 i = 1, nxpar
             params(i) = revpar(i)
 12       continue
       else
          ifft   = 1
          xgrid  = qgrid
          ygrid  = rgrid
          do 14 i = 1, nxpar
             params(i) = forpar(i)
 14       continue
       end if
c
c get default values of relevant program variables
c   scalars:
       xm_set = .false.
       myname = key
       dx1    = getsca(params(3),1)
       dx2    = getsca(params(4),1)
       if ((abs(dx1).le.tiny).and.(abs(dx2).le.tiny)) then
          dx1 = getsca(params(2),1)
          dx2 = dx1
       endif
       pc_path = 0
       pc_edge = blank
       pc_caps = .false.
       xmin   = getsca(params(5),1)
       xmax   = getsca(params(6),1)
       xw     = getsca(params(7),1)
       xmout  = getsca(params(11),1)
       outtyp = params(10)
       wintyp = params( 9)
c   strings:
       re_arr = undef
       im_arr = undef
       pc_arr = undef
       x_arr  = undef
       name1  = undef
       call gettxt(params(8), winnam)
       call gettxt('altwindow',winarr)
c parse command line
       call bkeys(str, mkeys, keys, values, nkeys)
       ndfkey    = 1
       defkey(1) = 'real'

       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if ((values(i).eq.undef).and.(i.le.ndfkey)) then
             values(i) = keys(i)
             keys(i)   = defkey(i)
          end if
          if ((keys(i).eq.'prefix').or.(keys(i).eq.'group')) then
             name1 = values(i)
          elseif (keys(i).eq.'altwindow') then
             winarr = values(i)
             call lower(winarr)
          elseif (keys(i).eq.'real') then
             re_arr = values(i)
             call lower(re_arr)
          elseif (keys(i).eq.'imag') then
             im_arr = values(i)
             call lower(im_arr)
          elseif (keys(i).eq.'phase_array') then
             pc_arr = values(i)
             call lower(pc_arr)
          elseif (keys(i).eq.'pc_edge') then
             pc_edge = values(i)
             pc_caps = .true.
          elseif (keys(i).eq.'pc_feff_path') then
             ier = iff_eval_in(values(i),pc_path)
          elseif (keys(i).eq.'pc_caps') then
             call str2lg(values(i), pc_caps, ier)
          elseif (keys(i).eq.'pc_full') then
             call str2lg(values(i), pc_caps, ier)
             pc_caps = .not.pc_caps
          elseif (keys(i).eq.params(1)) then
             x_arr = values(i)
             call lower(x_arr)
          elseif (keys(i).eq.params(2)) then
             ier = iff_eval_dp(values(i),dx1)
             dx2 = dx1
          elseif (keys(i).eq.params(3)) then
             ier = iff_eval_dp(values(i),dx1)
          elseif (keys(i).eq.params(4)) then
             ier = iff_eval_dp(values(i),dx2)
          elseif (keys(i).eq.params(5)) then
             ier = iff_eval_dp(values(i),xmin)
          elseif (keys(i).eq.params(6)) then
             ier = iff_eval_dp(values(i),xmax)
          elseif (keys(i).eq.params(7)) then
             ier = iff_eval_dp(values(i),xw)
          elseif (keys(i).eq.params(8)) then
             winnam = values(i)
             call lower(winnam)
          elseif (keys(i).eq.params(11)) then
             ier = iff_eval_dp(values(i),xmout)
             xm_set = .true.
          else
             messg = keys(i)(1:k)//' " will be ignored'
             call warn(1,
     $            ' *** '//myname//' : unknown keyword " '//messg)
          end if
 100   continue

c initialize fft work array (if not already done)
       if (.not.wftset) then
          call cffti(maxfft, wfftc)
          wftset = .true.
       end if
c
c get/resolve array names
c    if name1 not given, inherit from x_arr, re_arr, or im_arr.
       if (name1.eq.undef) then
          jdot = index(x_arr,'.')
          if (jdot.ne.0) name1 = x_arr(1:jdot-1)
       endif
       if (name1.eq.undef) then
          jdot = index(re_arr,'.')
          if (jdot.ne.0) name1 = re_arr(1:jdot-1)
       endif
       if (name1.eq.undef) then
          jdot = index(im_arr,'.')
          if (jdot.ne.0) name1 = im_arr(1:jdot-1)
       endif
       if (name1.eq.undef) then
          call warn(2, ' '//myname//': can''t determine group name')
          return             
       endif
       call fixnam(name1, 1)
       call lower(name1)

c evaluate all of the possible arrays
       do 180 i = 1, maxpts
          arr_x(i) = zero
          arr_r(i) = zero
          arr_i(i) = zero
          arr_w(i) = zero
          arr_p(i) = zero
 180   continue

       jxar = iff_eval(x_arr,  name1, arr_x, npts_x)
       jrea = iff_eval(re_arr, name1, arr_r, npts_r)
       jima = iff_eval(im_arr, name1, arr_i, npts_i)
       jwin = iff_eval(winarr, name1, arr_w, npts_w)
       jpha = iff_eval(pc_arr, name1, arr_p, npts_p)
c
c for phase-corrections given a feff file:
       if (pc_path .gt. 0) then
          jpha = 999
          call fefinp
          jfeff = jpthff(u2ipth(pc_path))
          ipos  = 0
cc          print*, ' pc : path = ', pc_path, pc_caps
          if (pc_caps) then 
             do 220 i = 1, maxpts
                xk = (i-1) * xgrid
                call lintrp(qfeff(1,jfeff), thcaps(1,jfeff),
     $               mffpts, xk, ipos, arr_p(i))
 220         continue
          else
             do 223 i = 1, maxpts
                xk = (i-1) * xgrid
                call lintrp(qfeff(1,jfeff), thepha(1,jfeff),
     $               mffpts, xk, ipos, arr_p(i))
 223         continue
          endif
       else if((pc_edge .ne. blank) .and.
     $         (pc_edge .ne. undef) ) then
          jpha = 99
          nptsxx = npts_x
          if (jxar.lt.1) then 
             do 240 i = 1, maxpts
                arr_x(i) = (i-1) * xgrid
 240         continue
             nptsxx = maxpts
          endif
          call feff_table_array(pc_edge,'caps',nptsxx,arr_x, arr_p)
       endif
       npts_p = npts_x

cc       print*, ' im_arr = ', im_arr(1:20), jima, arr_i(1), arr_i(5)
c if the x-array has been given, the real/imag/window data should 
c be re-interpolated onto an even x-grid starting at x=0.
       if (jxar.ge.1) then
          if (jrea.ge.1)
     $         call zgrid_array(arr_x, npts_x, arr_r, npts_r, xgrid)
          if (jima.ge.1) 
     $         call zgrid_array(arr_x, npts_x, arr_i, npts_i, xgrid)
          if (jwin.ge.1)
     $         call zgrid_array(arr_x, npts_x, arr_w, npts_w, xgrid)
          if (jpha.ge.1)
     $         call zgrid_array(arr_x, npts_x, arr_p, npts_p, xgrid)
       endif
       if ((jima.le.0).and.(jrea.le.0)) then
          call warn(2, ' '//myname//': no real or imaginary array?')
          return
       endif
       nxout = max(npts_r, npts_i)
cc       print*, ' n  = ', nxout, npts_x, npts_r, npts_i
       if (npts_x.gt.0)  nxout = min(npts_x,nxout)
       if (npts_r.gt.0)  nxout = min(npts_r,nxout)
       if (npts_i.gt.0)  nxout = min(npts_i,nxout)
c
c now generate the complex function for the fft
       do 200 i = 1, maxfft
          cinp(i) = dcmplx(arr_r(i), arr_i(i))
          cout(i) = dcmplx(zero, zero)
 200   continue
c       do i = 1, 5
c          print*, i, cinp(i)
c       end do

c  apply phase-correction, if requested
       if (jpha.gt.0) then
          call echo('doing phase corrected fft')
          do 250 i = 1, maxfft
             cinp(i) = cinp(i) * exp(cmi * arr_p(i))
 250      continue
       end if
c
c get window function if it wasn't alreary specified
cc       print*, ' jwin ', jwin
       if (jwin.le.0) then
          call window(winnam,dx1,dx2,xmin,xmax,xgrid,maxpts,arr_w)
       end if
c
c do the actual fft
cc       print*, ' to  xafsft  '
       call xafsft(maxfft,cinp,arr_w,xgrid,xw,wfftc,ifft,cout)
c
c  default rmax_out = 10.d0 (unless set explicitly)
       nyout = maxpts / 2
       if ((xmout.eq.0).and.(.not.xm_set)) then
          xmout= 30.d0
          if (ifft.eq.1) xmout= 10.d0
       endif
       if (xmout.gt.0) nyout = 1 + (xmout / ygrid)

c output program variables: 
c
c window array
c   if an x-array has been supplied, interpolate the evenly-spaced
c   window array used in the FT onto the provided x-array
       if (jxar.ge.1) then
          do 300 i = 1, maxpts
             txx(i) = (i-1) * xgrid
 300      continue
          ipos = 0
          do 310 i = 1, npts_x
             call lintrp(txx, arr_w, maxpts, arr_x(i),
     $            ipos, tmparr(i))
 310      continue
          call set_array(wintyp, name1, tmparr, npts_x, 1)          
       else
          call set_array(wintyp, name1, arr_w, nxout, 1)
       endif
c
c generate dp arrays from complex FT data
       nyout = min(nyout,maxpts)
       do 600 i = 1, nyout
          txx(i) = ygrid * (i-1)
          txr(i) = dble(cout(i))
          txi(i) = dimag(cout(i))
          txm(i) = sqrt(txr(i)*txr(i) + txi(i)*txi(i))
          txp(i) = atan2(txi(i),txr(i))
          if (i.gt.1) call pijump( txp(i),txp(i-1) )
 600   continue 
c x
cc       print*,'fft: ', outtyp
       call file_type_names(outtyp ,1, tnam)
       call set_array(tnam, name1, txx, nyout, 1)
c mag
       call file_type_names(outtyp, 4, tnam)
cc       print*,'fft: ',  tnam(1:20), nyout
       call set_array(tnam, name1, txm, nyout, 1)
c phase
       call file_type_names(outtyp, 5, tnam)
       call set_array(tnam, name1, txp, nyout, 1)
c real
       call file_type_names(outtyp, 2, tnam)
       call set_array(tnam, name1, txr, nyout, 1)
c imag
       call file_type_names(outtyp, 3, tnam)
       call set_array(tnam, name1, txi, nyout, 1)
c scalars:
cc       print*, ' fft set ', params(3), dx1
       call setsca(params(3),  dx1)
       call setsca(params(4),  dx2)
       call setsca(params(5),  xmin)
       call setsca(params(6),  xmax)
       call setsca(params(7),  xw)
       call setsca(params(11), xmout)
       call settxt(params(8),  winnam)

       return
c end subroutine iff_fft
       end
       subroutine zgrid_array(x, nx, y, ny, xgrid)
c
c interpolate x/y arrays onto even x-grid starting a x=0             
c overwrites y array and ny
       implicit none
       include 'consts.h'
       integer nx, ny
       double precision x(*), y(*), ta(maxpts), xt, xgrid
       integer  nx1, nt, ipos, i

       nx1 = min(nx, ny)
       nt  = 1 + x(nx1) / xgrid
       nt  = min(nt, maxpts)
       ipos = 0
       do 10 i = 1, nt
          xt = (i-1) * xgrid
          call lintrp(x, y, nx, xt, ipos, ta(i))
 10    continue
       ny = nt
       do 20 i = 1, ny
          y(i) = ta(i)
 20    continue 
       do 30 i = ny + 1, maxpts
          y(i) = 0
 30    continue 
       return
       end


