       subroutine iff_window(str)
c//////////////////////////////////////////////////////////////////////
c Copyright (c) 1997--2002 Matthew Newville, The University of Chicago
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
c purpose: ifeffit: generate xafs fourier transform window
c
c arguments:
c      str     command line to performd                 [in]
c
c requires: istrln, getsca, gettxt, bkeys, lower, str2re,
c           echo, cffti, window, lintrp, xafsft, setsca, settxt
c
c
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'fft.h'
       save
       integer  nxpar, ifft
       parameter (nxpar = 11)
       character*(*) str, defkey(3)*64, tnam*256
       character*256 x_arr, winnam*32
       character*256 name1, wintyp, outtyp, myname*5
       character*10  params(nxpar)

       double precision getsca, xmin, xmax, xw, dx1, dx2, xmout
       double precision xgrid
       double precision arr_x(maxpts), arr_w(maxpts), txx(maxpts)
       integer  npts_x, npts_r, npts_i, npts_w, npts_p, jxar,nptsxx
       integer  jwin, jrea, jima, jpha, i, k, jdot, idwin
       integer  ier,  istrln, ndfkey, ipos, nxout
       integer  iff_eval, iff_eval_dp, iff_eval_in
       logical  xm_set 
       external iff_eval, iff_eval_dp, iff_eval_in
       external getsca, istrln

       data (params(i), i=1, nxpar) /'k', 'dk', 'dk1', 'dk2',
     $      'kmin', 'kmax', 'kweight', 'kwindow', 'win', 'rsp',
     $      'rmax_out'/
c
c initialize 
c set default program variables to use based on "key"
       call iff_sync
       xgrid  = qgrid

c
c get default values of relevant program variables
c   scalars:
       xm_set = .false.
       myname = 'window'
       dx1    = getsca(params(3),1)
       dx2    = getsca(params(4),1)
       if ((abs(dx1).le.tiny).and.(abs(dx2).le.tiny)) then
          dx1 = getsca(params(2),1)
          dx2 = dx1
       endif
       xmin   = getsca(params(5),1)
       xmax   = getsca(params(6),1)
       xw     = getsca(params(7),1)
       xmout  = getsca(params(11),1)
       outtyp = params(10)
       wintyp = params( 9)
c   strings:
       x_arr  = undef
       name1  = undef
c parse command line
       call bkeys(str, mkeys, keys, values, nkeys)
       ndfkey    = 1
       defkey(1) = params(1)

       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if ((values(i).eq.undef).and.(i.le.ndfkey)) then
             values(i) = keys(i)
             keys(i)   = defkey(i)
          end if
          if ((keys(i).eq.'prefix').or.(keys(i).eq.'group')) then
             name1 = values(i)
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

c

c get/resolve array names
c    if name1 not given, inherit from x_arr, re_arr, or im_arr.
       if (name1.eq.undef) then
          jdot = index(x_arr,'.')
          if (jdot.ne.0) name1 = x_arr(1:jdot-1)
       endif
       if (name1.eq.undef) then
          call warn(2, ' '//myname//': can''t determine group name')
          return             
       endif
       call fixnam(name1, 1)
       call lower(name1)

       do 180 i = 1, maxpts
          arr_x(i) = zero
          arr_w(i) = zero
 180   continue
c evaluate the x array
       jxar = iff_eval(x_arr,  name1, arr_x, npts_x)
       if (jxar.le.0) then
          call warn(2, ' '//myname//': no x array?')
          return
       endif

c get window function 
       call window(winnam,dx1,dx2,xmin,xmax,xgrid,maxpts,arr_w)

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
       endif
c
c scalars:
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
