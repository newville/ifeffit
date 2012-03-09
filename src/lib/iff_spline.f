       subroutine iff_spline(str)
c
c purpose: ifeffit: xafs background removal -- driver for spline
c
c arguments:
c      key     command name to perform (spline)         [in]
c      str     command line to performd                 [in]
c
c notes:
c   1. driver for spline
c   2. if pre-edge appears to have not been done, it will be
c      done here
c
c requires: istrln, getsca, gettxt, setsca, settxt, bkeys,
c           echo, lower, str2dp, str2lg, spline, preedg
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
       include 'spline.h'
       save

       character*(*) str
       character*512 xmuarr, en_arr, name1, kstarr, cstarr, winnam*32
       character*512 bkgarr, ksparr, chiarr, outstr, tmps2
       double precision toler, xkmin, xkmax, xkw, dk1, dk2
       double precision pre1, pre2, enor1, enor2, getsca
       double precision xknots, splstf, de_min, cnorm(3), po, ps
       double precision arr_e(maxpts), arr_x(maxpts)
       double precision arr_cs(maxpts), arr_ks(maxpts)
       double precision a_bkg(maxpts), a_xk(maxpts), a_chi(maxpts)
       integer          npts_e, npts_x,  npts_ks, npts_cs, nnorm
       parameter (de_min=1.d-5)
       integer jen, jxmu,  ier, nknots, le, lx, l1, jdot, nclmp
       integer jbkg, jchi,  i, k, istrln, jksp, jcst, jkst
       logical fnorm, fixstd, do_std, vary_e0, do_pre, is_kev
       logical find_step, find_e0, do_spl
       logical           lclmp1, lclmp2
       double precision  xclmp1, xclmp2, r1stx
       character*64  defkey(3)
       integer       ndfkey, npts_k, iterp
       integer  iff_eval, iff_eval_dp, iff_eval_in, sort_xy
       external iff_eval, iff_eval_dp, iff_eval_in, sort_xy
       external  getsca, istrln
c
       call iff_sync
       
       lclmp1  = .false.
       lclmp2  = .false.
       nclmp   = 5
       iterp   = 2
       find_step = .true.
       vary_e0 = .true.
       find_e0 = .false.
       fnorm   = .false.
       fixstd  = .false.
       do_std  = .false.
       do_pre  = .true.
       do_spl  = .true.
       is_kev  = .false.
c      
c get default values for pre-edge parameters from current scalar values
       splstf = 1.d-4
cc       splstf = getsca('spl_stiff',1)
       e0    = getsca('e0',1)
       rbkg  = getsca('rbkg',1)
       r1stx = 2.d0
       toler = getsca('toler',1)
       xkmin = getsca('kmin_spl',1)
       xkmax = getsca('kmax_spl',1)
       xkw   = getsca('kweight_spl',1)
       dk1   = getsca('dk1_spl',1)
       dk2   = getsca('dk2_spl',1)
       pre1  = getsca('pre1',1)
       pre2  = getsca('pre2',1)
       enor1 = getsca('norm1',1)
       enor2 = getsca('norm2',1)
       nnorm = 3
       iprint = int(getsca('&print_level',0))

       winnam = ' '
       name1  = undef
       en_arr = undef
       xmuarr = undef
       kstarr = undef
       cstarr = undef
       call gettxt('kwindow',    winnam)
c
       xknots = zero
c
       call bkeys(str, mkeys, keys, values, nkeys)
c set default keywords for "undefined" sets
c ie: the first two unkeyed values will take keys 'energy', 'xmu'
       ndfkey    = 2
       defkey(1) = 'energy'
       defkey(2) = 'xmu'
c
       do 100 i = 1, nkeys
          k = istrln( keys(i))
c          if (iprint.gt.3)  print*, '>iff_spline|',keys(i)(1:k),
c     $         '|',values(i)(1:20),'|'
          if ((values(i).eq.undef).and.(i.le.ndfkey)) then
             values(i) = keys(i)
             keys(i)   = defkey(i)
          end if
          if ((keys(i).eq.'prefix').or.(keys(i).eq.'group')) then
             name1 = values(i)
          elseif ((keys(i).eq.'ee').or.(keys(i).eq.'e0'))  then
             ier = iff_eval_dp(values(i),e0)
             find_e0 = .false.
             vary_e0 = .false.             
          elseif (keys(i).eq.'rbkg') then
             ier = iff_eval_dp(values(i),rbkg)
          elseif (keys(i).eq.'r1st') then
             ier = iff_eval_dp(values(i),r1stx)
          elseif (keys(i).eq.'toler') then
             ier = iff_eval_dp(values(i),toler)
          elseif (keys(i).eq.'edge_step') then
             ier = iff_eval_dp(values(i),step)
             find_step = .false.
          elseif (keys(i).eq.'nknots') then
             ier = iff_eval_dp(values(i),xknots)
          elseif (keys(i).eq.'vary_e0') then
             call str2lg(values(i),vary_e0,ier)
          elseif (keys(i).eq.'spl_stiff') then
             ier = iff_eval_dp(values(i),splstf)
          elseif (keys(i).eq.'find_step') then
             call str2lg(values(i),find_step,ier)
          elseif (keys(i).eq.'find_e0') then
             call str2lg(values(i),find_e0,ier)
          elseif (keys(i).eq.'do_pre') then
             call str2lg(values(i),do_pre, ier)
          elseif (keys(i).eq.'do_spl') then
             call str2lg(values(i),do_spl, ier)
          elseif (keys(i).eq.'is_kev') then
             call str2lg(values(i),is_kev, ier)
          elseif (keys(i).eq.'fnorm') then
             call str2lg(values(i),fnorm,ier)
          elseif (keys(i).eq.'fixstd') then
             call str2lg(values(i),fixstd,ier)
          elseif (keys(i).eq.'kmin') then
             ier = iff_eval_dp(values(i),xkmin)
          elseif (keys(i).eq.'kmax') then
             ier = iff_eval_dp(values(i),xkmax)
          elseif (keys(i).eq.'kwindow') then
             winnam = values(i)
             call lower (winnam)
          elseif (keys(i).eq.'kweight') then
             ier = iff_eval_dp(values(i),xkw)
          elseif (keys(i).eq.'dk1') then
             ier = iff_eval_dp(values(i),dk1)
          elseif (keys(i).eq.'dk2') then
             ier = iff_eval_dp(values(i),dk2)
          elseif (keys(i).eq.'dk') then
             ier = iff_eval_dp(values(i),dk1)
             dk2 = dk1
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
          elseif (keys(i).eq.'interp') then
             iterp = 2
          elseif (keys(i).eq.'nclamp') then
             ier = iff_eval_in(values(i), nclmp)
          elseif (keys(i).eq.'clamp1') then
             lclmp1 = .true.
             call str2dp(values(i),xclmp1, ier)
          elseif (keys(i).eq.'clamp2') then
             lclmp2 = .true.
             call str2dp(values(i),xclmp2, ier)
          elseif (keys(i).eq.'energy') then
             en_arr = values(i)
             call lower(en_arr)
          elseif (keys(i).eq.'xmu') then
             xmuarr = values(i)
             call lower(xmuarr)
          elseif (keys(i)(1:7).eq.'chi_std') then
             cstarr = values(i)
             call lower(cstarr)
          elseif (keys(i)(1:5).eq.'k_std') then
             kstarr = values(i)
             call lower(kstarr)
          else
             messg = keys(i)(1:k)//' " will be ignored'
             call warn(1,' *** spline: unknown keyword " '//messg)
          end if
 100   continue
       do_std = ((cstarr.ne.undef).and.(kstarr.ne.undef))
       nknots = int(xknots)

c get/resolve array names
c    check that name1 exists.
c    otherwise, get it from xmuarr, if available
c    otherwise, give up.
cc       print*, ' spline : name1 = ', name1(1:40)
       if (name1.eq.undef) then
          jdot = index(xmuarr,'.')
          if (jdot.ne.0) name1 = xmuarr(1:jdot-1)
       endif
       if (name1.eq.undef) then
          jdot = index(en_arr,'.')
          if (jdot.ne.0) name1 = en_arr(1:jdot-1)
       endif
       if (name1.eq.undef) then
          call warn(2, ' spline: can''t determine group name')
          return             
       endif
       call fixnam(name1,1)
       call lower(name1)
       jdot = istrln(name1)

c input array names: energy, xmu -- both required
       jen   = iff_eval(en_arr, name1, arr_e, npts_e)
       jxmu  = iff_eval(xmuarr, name1, arr_x, npts_x)
       jkst  = iff_eval(kstarr, name1, arr_ks, npts_ks)
       jcst  = iff_eval(cstarr, name1, arr_cs, npts_cs)
c
cc       print*, 'spline ', jen, jxmu, jkst, jcst
       if (jen.le.0) then
          call warn(2, ' spline: no energy array?')
          return
       elseif(jxmu.le.0) then
          call warn(2, ' spline: no xmu array?')
          return
       elseif(npts_e.ne.npts_x) then
          call warn(2, ' spline: energy and xmu are different length?')
          return
       end if
c
c  if needed, call pre_edge for pre_edge subtraction
c
       if ((e0.le.arr_e(1)).or.(e0.ge.arr_e(npts_e))) find_e0 = .true.

c require that endat be monotonically increasing: if not, sort it
       ier =  sort_xy(arr_e, arr_x, npts_e, tiny)
       if (ier .eq. 1) then 
          call warn(1, ' spline:   energy data appears out of order')
cc          print*, 'npts_e = ', npts_e
       endif
c if obvious, or explicitly stated, switch KeV to eV:
       if (is_kev .or.
     $      ((arr_e(1).le.90).and.(arr_e(npts_e).le.90))) then
          call warn(1, ' spline:   energy data appears to be in keV')
          call kev2ev(arr_e, npts_e)
       endif
cc       print*, ' find_e0 vary_e0 = ', find_e0, vary_e0,  do_pre, e0
       if (do_pre .or. (abs(step).le.tiny) .or. find_e0) then
          le = max(1, istrln(en_arr))
          lx = max(1, istrln(xmuarr))
          l1 = max(1, istrln(name1))
          call setsca('e0', e0 )
          call setsca('pre1', pre1 )
          call setsca('pre2', pre2 )
          call setsca('norm1', enor1 )
          call setsca('norm2', enor2 )
          tmpstr = ', group = '//name1(1:l1)
cc          print*, ' find_e0 in pre_edge: ', find_e0, e0
          l1 = istrln(tmpstr)
          if (find_e0) then
             tmps2 = ', find_e0=true'
          else 
             write(tmps2,'(a,f10.3)') ', e0 =',e0
          endif
          tmpstr = tmpstr(1:l1)//tmps2
          l1 = istrln(tmpstr)
          if (abs(step).ge.tiny) then
             write(tmps2,'(a,f14.5)') ', edge_step =', step
             tmpstr = tmpstr(1:l1)//tmps2
          endif
          l1 = istrln(tmpstr)
          outstr = 'energy='//en_arr(1:le)//', xmu='
     $         //xmuarr(1:lx)//tmpstr(1:l1)
          l1 = istrln(outstr)
          write(tmps2,'(a,f13.4,a,f13.4,a,f13.4,a,f13.4,a,i3)') 
     $    ', pre1=', pre1,',pre2=',pre2,
     $    ',norm1=',enor1,',norm2=',enor2, ',norm_order=',nnorm
          lx = istrln(tmps2)
          outstr = outstr(1:l1)//tmps2(1:lx)
          if (iprint.gt.7) then
             call echo(' SPLINE: sending pre_edge:')
             call echo('    :  '//outstr)
          end if
          call iff_pre_edge(outstr)
       end if
c
c k and chi arrays for standard --
       if (do_std) then
          if (jkst.le.0) then
             call warn(2, ' spline: could not find k_std array?')
             return
          end if
          if (jcst.le.0) then
             call warn(2, ' spline: could not find chi_std array?')
             return
          end if
       end if

c  now the real call to spline
       call spline(arr_e, arr_x, npts_e, arr_ks, arr_cs, npts_ks,
     $      e0, rbkg, r1stx,toler, nknots, xkmin, xkmax,xkw,dk1,dk2,
     $      winnam, find_step, fnorm, nnorm, enor1, enor2, 
     $      pre1, pre2, step, lclmp1, xclmp1, lclmp2, xclmp2, nclmp,
     $      cnorm, fixstd, do_std, vary_e0, splstf, do_spl,
     $      a_bkg, npts_k, a_xk, a_chi)
c
c output array names:  bkg, k, chi
       call set_array('bkg', name1, a_bkg, npts_e, 1)
       call set_array('k',   name1, a_xk,  npts_k, 1)
       call set_array('chi', name1, a_chi, npts_k, 1)
c  
c
       po = getsca('pre_offset',1)
       ps = getsca('pre_slope',1)

       do 200 i = 1, npts_e
          tmparr(i) = (arr_x(i) - po - ps * arr_e(i))/step
 200   continue 
       call set_array('norm', name1, tmparr, npts_e, 1)

       xknots = nknots
       call setsca('nknots',      xknots)
       call setsca('rbkg',        rbkg )
       call setsca('e0',          e0 )
       call setsca('kweight_spl', xkw)
       call setsca('kmin_spl',    xkmin )
       call setsca('kmax_spl',    xkmax )
       call setsca('dk1_spl',     dk1   )
       call setsca('dk2_spl',     dk2   )
       call setsca('toler',       toler )
       call setsca('edge_step',   step )
       call setsca('norm_c0',    cnorm(1))
       call setsca('norm_c1',    cnorm(2))
       call setsca('norm_c2',    cnorm(3))
       return
c end  subroutine iff_spline
       end
