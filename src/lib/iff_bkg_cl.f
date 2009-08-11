       subroutine iff_bkg_cl(str)
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
c purpose: ifeffit pre-edge subtraction, normalization and exafs
c          extraction based on CL f'' data
c
c arguments:
c      str     command line to performd                 [in]
c
c 
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'spline.h'
       save
       character*(*)   str, defkey(3)*64
       character*512   path, name1
       character*512   xmuarr, en_arr, prearr, norarr
       integer         lenwrk, lminfo, mxvars
       parameter  (mxvars=5)
       parameter  (lenwrk = (mxvars+1)*maxpts + 5*mxvars)
       double precision  work(lenwrk), fv(maxpts), varys(mxvars)
       double precision  arr_f1(maxpts),arr_f2(maxpts),tmpar2(maxpts)
       integer           iwork(mxvars), nvarys
       double precision  pre1, pre2, enor1, enor2, toler
       double precision  slope, offset, cnorm(3)
       double precision  ewid, a0, a1, a2, st,ex
       double precision  e0_dt, es_dt, ps_dt, po_dt, estep
       double precision  wgt_pre, wgt_xan, wgt_exa, qmax, xtmp
       logical   stfind, eefind, is_kev

       integer   jen, jxmu, ier, i, k, istrln, jdot, j
       integer   ndfkey, npts_e, npts_x, nnorm, iz, ilen
       integer   clcalc,  iret, npts_k, ipos, iterp
       integer   ne0, npts_t
       integer   iff_eval, iff_eval_dp, iff_eval_in, sort_xy
       external  iff_eval, iff_eval_dp, iff_eval_in, clcalc
       external  istrln, clbfun, sort_xy
c
c get default values for pre-edge parameters from current scalar values
       call iff_sync
       eefind = .false.
       stfind = .true.
       is_kev = .false.
       estep  = one
       iz     = 0 
       e0     = 0 
       step   = 0 
       pre1   = 0 
       pre2   = 0 
       enor1  = 0 
       enor2  = 0 
       slope  = 0 
       offset = 0 
       nnorm  = 3
       ewid   = 5.d-1
       wgt_pre= 1.d1
       wgt_exa= 1.d0
       wgt_xan= 1.d-4
       iterp  = 2
       name1  = undef
       call gettxt('group', name1)
       call bkeys(str, mkeys, keys, values, nkeys)
       
c set default keywords for "undefined" sets
c ie: the first two unkeyed values will take keys 'energy', 'xmu'
       ndfkey    = 3
       defkey(1) = 'energy'
       defkey(2) = 'xmu'
       defkey(3) = 'z'
       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if ((values(i).eq.undef).and.(i.le.ndfkey)) then
             values(i) = keys(i)
             keys(i)   = defkey(i)
          end if
          if ((keys(i).eq.'iz').or.(keys(i).eq.'z'))  then
             ier = iff_eval_in(values(i),iz)
          elseif ((keys(i).eq.'ee').or.(keys(i).eq.'e0'))  then
             ier = iff_eval_dp(values(i),e0)
             eefind = eefind .or. (values(i)(1:1).eq.'?')
          elseif (keys(i).eq.'width') then
             call str2dp(values(i),ewid, ier)
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
          elseif (keys(i).eq.'interp') then
             iterp = 2
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
             call warn(1,' *** bkg_cl: unknown keyword " '//messg)
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
          call warn(2,' bkg_cl: can''t determine group name')
          return             
       endif
       call fixnam(name1,1)
       call lower(name1)
       npts_e = maxpts
       npts_x = maxpts
       jdot   = istrln(name1)
       jen    = iff_eval(en_arr, name1, endat,  npts_e)
       jxmu   = iff_eval(xmuarr, name1, xmudat, npts_x)

       if (jen.le.0) then
          call warn(2,' bkg_cl: no energy array?')
          return
       elseif(jxmu.le.0) then
          call warn(2,' bkg_cl: no xmu array?')
          return
       end if
c
       do 25 i = 1, npts_e
          arr_f1(i) = zero
          arr_f2(i) = zero
 25    continue 
c
c require that endat be monotonically increasing: if not, sort it
       ier =  sort_xy(endat, xmudat, npts_e, tiny)
       if (ier .eq. 1) then 
          call warn(1,' bkg_cl:   energy data appears out of order')
       endif
c if obvious, or explicitly stated, switch KeV to eV:
       if (is_kev .or.
     $      ((endat(1).le.90).and.(endat(npts_e).le.90))) then
          call warn(1,' bkg_cl:   energy data appears to be in keV')
          call kev2ev(endat, npts_e)
       endif

c call preedg for data, just to get internal e0 value
cc       print*,  ' in bkg_cl before preedg call: ', eefind, e0
       eefind = .true.
       call preedg(eefind, stfind, npts_x, endat, xmudat,
     $      e0, pre1, pre2, enor1, enor2, nnorm,
     $      step,slope,offset,cnorm)
       e0_dt = e0
       es_dt = step
       ps_dt = slope
       po_dt = offset
c
c call clcalc
       call gettxt('&install_dir', path)
       ilen = istrln(path)
       path = path(1:ilen)//'/cldata/'
       iret = clcalc(iz, path, npts_e, endat, arr_f1, arr_f2)
       estep= one
       call conv_lor(ewid, npts_e, endat, arr_f2, estep, spldat)
       eefind = .true.
       call preedg(eefind, stfind, npts_x, endat, spldat,
     $      e0, pre1, pre2, enor1, enor2, nnorm,
     $      step,slope,offset,cnorm)
       varys(1) = zero
       varys(2) = zero
       varys(3) = zero
       varys(4) = es_dt/(step+0.01d0)
c
c  make weighting factor
       do 200 i = 1, npts_x
          a0   = wgt_exa
          if (endat(i) .lt. e0_dt)           a0 = wgt_pre
          if (abs(endat(i) - e0_dt).le.50d0) a0 = wgt_xan
          splfit(i) = a0
 200   continue 
       
       nvarys   = 4
       lminfo   = 0
       do 210 i = 1, npts_e
          fv(i) = zero
 210   continue
       do 220 i = 1, lenwrk
          work(i) = zero
 220   continue
       do 230 i = 1, mxvars
          iwork(i) = 0
 230   continue 
       toler = 1.d-5
       call lmdif1 (clbfun, npts_e, nvarys, varys, fv,
     $      toler, lminfo, iwork, work, lenwrk)

c
c  make array of pre-edge subtracted data
c  $group.pre: output (ensuring that it doesn't overwrite xmu!)
       a0 = varys(1)
       a1 = varys(2)
       a2 = varys(3)
       st = varys(4)
       do 730 i = 1, npts_x
          tmparr(i) = a0 + endat(i)*(a1 + a2*endat(i)) + spldat(i)*st
 730   continue 
c
c  finally, call preedg on modified f" data, extending
c  the normalization range out in energy
       eefind = .true.
       a0     = -e0 + (enor2 + e0 + endat(npts_x)) / 2

       call preedg(eefind, stfind, npts_x, endat, tmparr,
     $      e0, pre1, pre2, enor1, a0, nnorm,
     $      step,slope,offset,cnorm)

c  make pre-edge subtracted data and pre-edge subtracted f"
       do 760 i = 1, npts_x
          ex        = po_dt + endat(i) * ps_dt
          xmudat(i) = xmudat(i) - ex 
          tmpar2(i) = xmudat(i)/step
          spldat(i) = tmparr(i) - ex
          splfit(i) = spldat(i)/step
 760   continue 

       prearr = name1(1:jdot)//'.pre'
       norarr = name1(1:jdot)//'.norm'
       call set_array(prearr, name1, xmudat, npts_x, 1)
       call set_array(norarr, name1, tmpar2, npts_x, 1)

       prearr = name1(1:jdot)//'.f2'
       call set_array(prearr, name1, tmparr, npts_x, 1)
       prearr = name1(1:jdot)//'.f2pre'
       norarr = name1(1:jdot)//'.f2norm'
       call set_array(prearr, name1, spldat, npts_x, 1)
       call set_array(norarr, name1, splfit, npts_x, 1)

c
c create chi(k) from this background
c
c tmparr holds q(E_i), tmpar2 holds chi(E_i)
       do 900 i = 1, npts_x
          tmpar2(i) =(xmudat(i) - spldat(i)) / step
 900   continue 

       call chie2k(endat, tmpar2, npts_x, e0_dt, 
     $      npts_k, arr_f1, arr_f2)

       call set_array('k',   name1, arr_f1,  npts_k, 1)
       call set_array('chi', name1, arr_f2,  npts_k, 1)

c reset the program variables
       call setsca('e0',         e0_dt)
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
c end  subroutine iff_bkg_cl
       end

       subroutine chie2k(en, chie, ne, e0,  nk,xk,chik)
c
       include 'consts.h'
       integer          ne, nkmax, nk, i, j, ne0, nt
       double precision en(*), chie(*), xk(*), chik(*)
       double precision e0,  qmax
       double precision tx(maxpts), ty(maxpts)
       integer   nofxa
       external  nofxa
       ne0 = nofxa(e0, en, ne)

c       print*, 'chie2k ne, ne0 ', ne, ne0
       j = 0
       do 10 i = ne0, ne
          if (en(i) .ge. e0) then
             j = j + 1
             tx(j)  = sqrt(etok * abs(en(i)-e0))
             ty(j)  = chie(i)
          endif
 10   continue 
       nt   = j
       qmax = tx(nt)
       nk   = min(maxpts, int((qmax + 1.d-2) /qgrid))
c       print*, 'chie2k x,y ', tx(1), tx(2), ty(1), ty(2)
c       print*, 'chie2k nk = ',nk
       do 20 i = 1, nk
          xk(i) = (i-1) * qgrid
 20    continue
       call grid_interp(tx, ty, nt, zero, qgrid, nk, chik)
       return
       end



