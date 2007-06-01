       subroutine fitfun(mf, nx, xvar, fvec, iend)
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
c purpose:  feffit fitting function for lmdif1 minimization 
c
c arguments:
c      mf       number of points in ffit             [in]
c      nx       number of points in xv               [in]
c      xv       vector of variables                  [in]
c      fvec     output vector to be minimized        [out]
c      iend     information tag                      [out]
       implicit none
       include 'consts.h'
       include 'arrays.h'
       include 'keywrd.h'
       include 'fft.h'
       include 'fefdat.h'
       include 'pthpar.h'
       include 'feffit.h'
c local variables
       integer    nx, mf, iend, i,  id, isp, nx1, iter
       integer    jfit, nqdata, nfit1, nkpar, npaths
       integer    iupath(mpaths), inxx, ixx, joff, jqw
       integer   ier, iff_eval_dp
       double precision xvar(nx), fvec(mf), xolow, xohigh, bvalue
       double precision getsca, xspl(mtknot), bx, qx, sum, tmpval
       double precision xsum, sumsqr
       character  arg*2 
       integer    isarg, ilen, istrln, j, nmx
       external   bvalue, getsca,  istrln, iff_eval_dp, sumsqr
       save
c
       nkpar = 0
       id    = 1
       if (nx.ne.nvarys) iend = 1
       if (mf.ne.mfit)   iend = 2
       nx1 = nx - nbkg(id)
cc       print*, 'FITFUN N_vars:',nx,mf,nfdats
       do 20 i = 1, nx
          scalar(i) = xvar(i)
 20    continue
       call synvar
cc       print*, 'VARS ', xvar(1), xvar(2), xvar(3), xvar(4)
       if (final) rfact_total = zero
c
c  sum function to minimize over data sets
c   jfit is the counter (through all the data sets)
c   for the total number of fitting points

       jfit = 0
       do 3000 id = 1, nfdats
          nqdata = min(maxpts, max(2, nqfit(id)) + 10)
          if (ifft(id).eq.1) then
             xolow  = rmin(id)
             xohigh = rmax(id)
          else
             xolow  = qmin(id)
             xohigh = qmax(id)
          endif
          do 200 i = 1, nfit(id)*nqwfs(id)
             thifit(i)   = zero
 200      continue
c  re-initialize array for theoretical chi(k)
c  by assigning this to the background function
          do 300 i = 1, nqdata
             thiq(i, id) = zero
             thiqr(i, id) = zero
 300      continue
c
c sum over paths for theory chi for this data set
          npaths = 0
          do 400 i = 1, mpaths
             iupath(i) = 0
             if (iulist(i,id).ne.0) then
                npaths = npaths + 1
                iupath(npaths) = iulist(i,id)
             end if
 400      continue 
          call sum_paths(id, iupath, npaths, nqdata,
     $         thiqr(1,id), thiq(1,id))
c
c if refining background, include that here
          if ( bkgfit(id))  then
             do 450 isp = 1, nbkg(id)
                write(tmpstr, '(a,i2.2,a,i2.2)') 'bkg',id,'_',isp
                xspl(isp) = getsca(tmpstr,0)
cc                print*, 'FITFUN i ' , isp, xspl(isp)
 450         continue
             do 470 i = 1, nqdata
                qx = qgrid*(i-1)
                thiq(i, id) = thiq(i, id) + 
     $               bvalue(qknot(1,id), xspl, nbkg(id),korder,qx,0)
 470         continue 
          end if
c
c subtract data from theory
          do 500 i = 1, nqdata
             thiq(i,id) = thiq(i,id)-chiq(i,id)
 500      continue 
c
c loop over k-weights for multiple k-weights
c    top down, so that the first k-weight listed is done last
c
          do 1000 jqw = nqwfs(id), 1, -1
             qweigh(id) = qwfs(jqw,id)

c   take fft of theory(+bkg) - data
c   note: imag part of theory chi(k) compares to real part of data!!!
c   see ff2chi as well
             call fitfft(thiq(1,id), maxpts, maxfft, wfftc, qgrid,
     $            qwindo(1,id), qwfs(jqw,id), rwindo(1,id), one,
     $            ifft(id),xolow,xohigh, nfit1, thifit)

             if (nfit1.ne.nfit(id)) then
                call warn(3,' fitfun fitfft failed internal test.')
                iend = -10
             end if
c
c  evaluate the contribution to fvec for this data set.  weight scales
c  chi-square properly to the number of independent points. this is
c  important for error analysis (if chi-square is to increase by one,
c  it  must be scaled correctly.), but only in the final pass, when
c  chi-square and r-factors will be calculated.
c             print*, 'fitfun ', jfit, jqw, id,
c     $            weight(jqw, id), nfit(id), thifit(1), thifit(2)
             do 700 i =  1, nfit(id)
                fvec(jfit+i) = (thifit(i))/weight(jqw,id)
 700         continue

             jfit = jfit + nfit(id)

c construct r-factor
             if (final) then 
                call fitfft(chiq(1,id), maxpts, maxfft, wfftc, qgrid,
     $               qwindo(1,id), qwfs(jqw,id), rwindo(1,id), one, 
     $               ifft(id),xolow,xohigh, nfit1, chifit)
                if (nfit1.ne.nfit(id)) then
                   call warn(3,' fitfun fitfft failed internal test.')
                   iend = -10
                end if

                sum        = zero
                rfactr(id) = zero
                do 800 i = 1, nfit(id)
                   sum        = sum        + chifit(i)*chifit(i)
                   rfactr(id) = rfactr(id) + thifit(i)*thifit(i)
 800            continue
                if (sum.le.tiny) sum = tiny
                rfactr(id)  =  rfactr(id) / (sum * nqwfs(id))
                rfact_total = rfact_total + rfactr(id) 
cc                print*, ' final ', sum, rfactr(id), id, rfact_total
             end if
c
c  restraints
             if (nrestraint(id).ge.1) then
cc                print*, ' RESTRAINT ', nrestraint(id), id, jfit
                do 900 i =  1, nrestraint(id)
                   if ( (restraint(i,id) .ne. undef).and.
     $                  (restraint(i,id) .ne. '')) then 
                      ier    = iff_eval_dp(restraint(i,id),tmpval)
                      if (ier.eq.0) then
                         jfit   = jfit + 1
                         fvec(jfit) = tmpval
cc                         print*, ' t: ', tmpval, jfit, ier
                      endif
                   endif
 900            continue
             endif
 1000     continue   ! end loop over k-weights
cc          print*, ' end of ndata loop  ', id, nfit(id), jfit
 3000  continue      ! end loop over data sets

       if (final) rfact_total = rfact_total / max(1,nfdats)

c
c execute user-defined macro
       iter  =  int(getsca('&fit_iteration',0))
       if (iter.gt.itera) then
          itera = iter
          xsum = sumsqr(fvec,jfit)
cc          print*, ' iteration ', iter, mf,mfit,jfit,nfdats, xsum
          if (ifit_mac.gt.0) then
             call iff_macro_do(ifit_mac, fit_m_arg, 0,.false.)
          end if

       end if
       return
c end subroutine fitfun
       end


