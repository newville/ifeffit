       subroutine genfun(mf, nx, xv, fv, iend)
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
c purpose:  generic function for lmdif1 minimization 
c
c arguments:
c      mf       number of points in ffit                [in]
c      nx       number of points in xv                  [in]
c      xv       vector of variables                     [in]
c      fv       output vector to be minimized           [out]
c      iend     information tag                         [out]
c
       implicit none
       include 'consts.h'
       include 'arrays.h'
       include 'feffit.h'
       include 'keywrd.h'
c local variables
       integer   nx, mf, iend, i, j, iter, nf1, ne1, get_array
       integer   ier, iff_eval_dp, mfx
       double precision  xv(nx), fv(mf), epsmin, getsca, tmpval
       double precision  fitarr(maxpts)
       parameter (epsmin = 1.d-9)
       external getsca, get_array, iff_eval_dp
       save
       if (nx.ne.nvarys) iend = 1
       if (mf.ne.mfit)   iend = 2
       do 20 i =1, nx
          scalar(i) = xv(i)
 20    continue
c
c synchronize variables and their dependencies
       call synvar
c
c look up function to minimize
       nf1 = get_array(cfmin_arr, cfmin_pre, 0, fitarr)
       ne1 = get_array(cfmin_err, cfmin_pre, 0, tmparr)
cc       print*, ' genfun = ', nf1, ne1, ':', cfmin_arr(1:40), '/',
cc     $      cfmin_err(1:40)
cc       print*, ' mf     = ', mf, ifitx1, ifitx2, usewgt,ne1
cc       print*, ' tmparr = ', tmparr(1), tmparr(2),tmparr(3)

       mfx = mf - nrestraint(1)
       if (usewgt.and.(ne1.gt.0)) then
          do 50 i = 1, mfx 
             j = ifitx1 - 1 + i 
             fv(i) = fitarr(j) / max(epsmin, tmparr(j))
 50       continue
       else
          do 70 i = 1, mfx
             j = ifitx1 - 1 + i 
             fv(i) = fitarr(j)
 70       continue
       end if
       if (nrestraint(1) .ge. 1) then
          do 90 i =  1, nrestraint(1) 
             if ((restraint(i,1).ne.'').and.
     $            (restraint(i,1).ne.undef)) then
                ier       = iff_eval_dp(restraint(i,1),tmpval)
                fv(mfx+i) = tmpval
             endif
 90       continue 
       endif

c
c execute user-defined macro
       iter  =  int(getsca('&fit_iteration',0))
       if ((iter.gt.itera) .and. (ifit_mac.gt.0)) then
          itera = iter
          call iff_macro_do(ifit_mac, fit_m_arg, 0,.false.)
       endif
       return
c end subroutine genfun
       end
