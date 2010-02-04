c -*-fortran-*-
       subroutine iff_pre_edge(str)
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
c purpose: ifeffit pre-edge removal, normalization of xafs data
c          and guessing e0  
c
c arguments:
c      str     command line to performd                 [in]
c
c notes:
c   1. driver for preedg
c
c requires: istrln, getsca, gettxt, setsca, settxt, bkeys,  
c           echo, lower, str2re, str2lg, preedg
c 
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'spline.h'
       save
       character*(*)  str, defkey(3)*64
       character*512     xmuarr, en_arr, prearr, norarr, name1
       double precision  pre1, pre2, enor1, enor2
       double precision slope, offset, cnorm(3), xn
       double precision  arr_e(maxpts), arr_x(maxpts), getsca
       double precision tmpar2(maxpts)
       logical   stfind, eefind, is_kev
       integer   jen, jpre, jxmu, ier, i, k, istrln, jdot
       integer   ndfkey, npts_e, npts_x, nnorm
       integer  iff_eval, iff_eval_dp, iff_eval_in, sort_xy
       external iff_eval, iff_eval_dp, iff_eval_in, sort_xy
       external  getsca, istrln
c
c get default values for pre-edge parameters from current scalar values
       call iff_sync
       eefind = .false.
       stfind = .true.
       e0     = 0 ! getsca('e0',1)
       step   = 0 ! getsca('edge_step',1)
       pre1   = 0 ! getsca('pre1',1)
       pre2   = 0 ! getsca('pre2',1)
       enor1  = 0 ! getsca('norm1',1)
       enor2  = 0 ! getsca('norm2',1)
       slope  = 0 ! getsca('pre_slope',1)
       offset = 0 ! getsca('pre_offset',1)
       nnorm  = 3 
       name1  = undef
c       print*, 'IFF_PRE :'
c       print*, str
       call bkeys(str, mkeys, keys, values, nkeys)

c set default keywords for "undefined" sets
c ie: the first two unkeyed values will take keys 'energy', 'xmu'
       ndfkey    = 2
       defkey(1) = 'energy'
       defkey(2) = 'xmu'
       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if ((values(i).eq.undef).and.(i.le.ndfkey)) then
             values(i) = keys(i)
             keys(i)   = defkey(i)
          end if
          if ((keys(i).eq.'ee').or.(keys(i).eq.'e0'))  then
             ier = iff_eval_dp(values(i),e0)
             eefind = eefind .or. (values(i)(1:1).eq.'?')
          elseif (keys(i).eq.'group') then
             name1 = values(i)
          elseif (keys(i).eq.'pre1') then
             ier = iff_eval_dp(values(i),pre1)
          elseif (keys(i).eq.'pre2') then
             ier = iff_eval_dp(values(i),pre2)
          elseif (keys(i).eq.'norm1') then
             ier = iff_eval_dp(values(i),enor1)
          elseif (keys(i).eq.'norm2') then
             ier = iff_eval_dp(values(i),enor2)
          elseif (keys(i).eq.'norm_order') then
             ier = iff_eval_in(values(i), nnorm)
          elseif (keys(i).eq.'edge_step') then
             ier = iff_eval_dp(values(i),step)
             stfind = .false.
          elseif (keys(i).eq.'is_kev') then
             call str2lg(values(i),is_kev, ier)
          elseif ((keys(i).eq.'find_e0').or.
     $            (keys(i).eq.'e0find')) then
             call str2lg(values(i),eefind,ier) 
          elseif (keys(i).eq.'energy') then
             en_arr = values(i)
             call lower(en_arr)
          elseif (keys(i).eq.'xmu') then
             xmuarr = values(i)
             call lower(xmuarr)
          else
             messg = keys(i)(1:k)//' " will be ignored'
             call warn(1,' *** pre_edge: unknown keyword " '//messg)
          end if
 100   continue
c
c get/resolve array names
       if (name1.eq.undef) then
          jdot = index(xmuarr,'.')
          if (jdot.ne.0) name1 = xmuarr(1:jdot-1)
       endif
       if (name1.eq.undef) then
          jdot = index(en_arr,'.')
          if (jdot.ne.0) name1 = en_arr(1:jdot-1)
       endif
       if (name1.eq.undef) then
          call warn(2, ' pre_edge: can''t determine group name')
          return             
       endif
       call fixnam(name1,1)
       call lower(name1)
       jdot = istrln(name1)
       jen   = iff_eval(en_arr, name1, arr_e, npts_e)
       jxmu  = iff_eval(xmuarr, name1, arr_x, npts_x)
       if (jen.le.0) then
          call warn(2, ' pre_edge: no energy array?')
          return
       elseif(jxmu.le.0) then
          call warn(2, ' pre_edge: no xmu array?')
          return
       end if

c require that endat be monotonically increasing: if not, sort it
       ier =  sort_xy(arr_e, arr_x, npts_e, tiny)
       if (ier .eq. 1) then 
          call warn(1,' pre_edge: energy data appears out of order')
       endif

       if (is_kev .or.
     $      ((arr_e(1).le.90).and.(arr_e(npts_e).le.90))) then
          call warn(1,' pre_edge: energy data appears to be in keV')
          call kev2ev(arr_e, npts_e)
       endif
c
c  do real call to pre-edge
cc      print*,  ' in pre_edge before preedg call: ', eefind, e0
       call preedg(eefind, stfind, npts_x, arr_e, arr_x,
     $      e0, pre1, pre2, enor1, enor2, nnorm,
     $      step,slope,offset,cnorm)
cc       print*,  ' after preedg call: ',e0,step,slope,offset,cnorm
c
c  make array of pre-edge subtracted data
c  $group.pre: output (ensuring that it doesn't overwrite xmu!)
       prearr = name1(1:jdot)//'.pre'
       norarr = name1(1:jdot)//'.norm'
       if (prearr.eq.xmuarr) then
          prearr = name1(1:jdot)//'.pre_edge'
          norarr = name1(1:jdot)//'.norm_1'
       endif

       do 200 i = 1, npts_x
          tmparr(i) = arr_x(i) - offset - slope * arr_e(i)
          tmpar2(i) = tmparr(i)/step
 200   continue 
       call set_array(prearr, name1, tmparr, npts_x, 1)
       call set_array(norarr, name1, tmpar2, npts_x, 1)
c
c reset the program variables
       call setsca('e0',         e0)
       call setsca('edge_step',  step)
       call setsca('pre1',       pre1)
       call setsca('pre2',       pre2)
       call setsca('norm1',      enor1)
       call setsca('norm2',      enor2)
       call setsca('pre_slope',  slope)
       call setsca('pre_offset', offset)
       call setsca('norm_c0',    cnorm(1))
       call setsca('norm_c1',    cnorm(2))
       call setsca('norm_c2',    cnorm(3))
       call settxt('group',      name1)
       return
c end  subroutine iff_pre_edge
       end

