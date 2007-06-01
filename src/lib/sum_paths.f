       subroutine sum_paths(idata, iupath, nxpath, nqmax,
     $      ckreal, ckimag)
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
c  sum-over-paths for a particular data set, returning 
c      real and imaginary parts of chi(k)
c     used by iff_ff2chi and fitfun
c
       implicit none
       include 'consts.h'
       integer       idata, i, ip, nqmax, xafs_path, ret
       integer       nxpath, iupath(mpaths)
       double precision  xdsave, xtmp, getsca, reff
       double precision  ckreal(maxpts), ckimag(maxpts) 
       double precision  tmpi(maxpts), tmpr(maxpts)
       external    getsca, xafs_path

       do 100 i = 1, maxpts
          ckreal(i)  = zero
          ckimag(i)  = zero
 100   continue
       xdsave = getsca('data_set',0)
       xtmp   = one*max(1,min(mdata,idata))
       call setsca('data_set',  xtmp)
cc       print*, 'sum paths data_set', xtmp
       do 1000 ip = 1, nxpath
          ret = xafs_path(iupath(ip), tmpr, tmpi, reff)
cc          print*, ' path ', ip, iupath(ip), ret, reff
          if (ret .eq. 1) then
c  add this to the other paths
             do 850 i = 1, nqmax
                ckreal(i) = ckreal(i) + tmpr(i)
                ckimag(i) = ckimag(i) + tmpi(i)
 850         continue
c          else 
c             print*, 'sum over paths: path not used: ',
c     $            ip, iupath(ip)
          endif
 1000  continue
c now return previous the previous 'user variables'
       call setsca('data_set',  xdsave)
       return
       end

       integer function xafs_path(ipath,chi_r, chi_i,reff)
c
c  calculate xafs for a single path 
c  ipath =  'user path index' 
c  idata =  'data set'
c  reff <= 0. on output means path isn't defined.
c
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'fefdat.h'
       include 'feffit.h'
       include 'pthpar.h'
       save
       integer ipath, i, j, k, inpath, nqmax, jfeff
       integer u2ipth, ntmp, nkpar, icode(micode)
       double precision reff, degen, xtmp, getsca
       double precision chi_r(maxpts),chi_i(maxpts)
       double precision tpakar(maxpts),tpaamp(maxpts),tpapha(maxpts)
       external         u2ipth, getsca

       reff      = -one
       xafs_path = 0
       inpath    = u2ipth(ipath)
cc       print*, ' xafs path ', ipath, u2ipth(ipath)
       if (inpath.le.0) return
       if (jpthff(inpath).le.0) then
          write(tmpstr,'(1x,a,i5)') ' no FEFF file for path ', ipath
          call warn(2,tmpstr)
          return
       else
          jfeff = jpthff(inpath)
          reff  = refpth(jfeff)
          degen = degpth(jfeff)
          xtmp  = ipath
          call setsca('path_index', xtmp)
          call setsca('degen', degen)
          call setsca('reff',  reff)
       endif
       call synvar
c      
c path OK to use
       xafs_path = 1
       nkpar     = 0

       do 340 i = 1, maxpts
          tpakar(i) = (i-1)* qgrid
          tpaamp(i) = one
          tpapha(i) = zero
 340   continue 
          
c loop over path parameters to evaluate
       do 500 i = 1, mpthpr
c
c set default path params here
          tmparr(1) = zero
          if (i.eq.jfps02)  then
             tmparr(1) = one
          elseif (i.eq.jfpdeg) then
             tmparr(1) = degpth(jfeff)
          endif
          ntmp = 0
          do 420 k = 1, micode
             icode(k)  = icdpar(k,i,inpath)
 420      continue 
          if (iprint.ge.12)
     $          call rpndmp(icode)
c  evaluate parameter if it was defined
          if (icode(1).ne.0) then
             call decod(icode, micode, 
     $            consts, scalar, array, narray, nparr, 
     $            maxsize_array, maxarr,  ntmp, tmparr)
          end if
c
c set the param value from tmparr:
          param(i) = tmparr(1)
          if (i.eq.jfpkar) then
             if (nkpar.gt.0)  nkpar = min(nkpar, ntmp)
             if (nkpar.eq.0)  nkpar = ntmp
             do 470 j = 1, ntmp
                tpakar(j) = tmparr(j)
 470         continue 
          elseif (i.eq.jfpaar) then
             if (nkpar.gt.0)  nkpar = min(nkpar, ntmp)
             if (nkpar.eq.0)  nkpar = ntmp
             do 480 j = 1, ntmp
                tpaamp(j) = tmparr(j)
 480         continue 
          elseif (i.eq.jfppar) then
             if (nkpar.gt.0)  nkpar = min(nkpar, ntmp)
             if (nkpar.eq.0)  nkpar = ntmp
             do 490 j = 1, ntmp
                tpapha(j) = tmparr(j)
 490         continue 
          endif
 500   continue

c
c   get chi(k) for this path from feff and path parameters
c
       if ( (inpath.gt.0).and.(jfeff.gt.0))  then
          if (iprint.ge.9) then
             call echo('calling chipth:')
c      c             print*, '   jfeff, maxpts   =', jfeff, maxpts, mffpts
c      c             print*, '   reff, degen     =', reff, degen
c      c          print*, 'sum_paths: jfeff, nffpts   =',
c      c     $         jfeff,nffpts(jfeff)
          end if

cc       print*, 'Phase array: ', nkpar, tpapha(1), tpapha(3)

          call chipth(theamp(1,jfeff), thepha(1,jfeff),
     $         qfeff(1,jfeff), xlamb(1,jfeff), realp(1,jfeff),
     $         nffpts(jfeff), reff, nkpar, tpakar, tpaamp, tpapha,
     $         maxpts, chi_r, chi_i)
          
cc          print*, ' path ', ipath, inpath, param(jfpdeg)
c          print*, 'degen,s02,e0,ei =',degen,
c     $         param(jfps02), param(jfpe0), param(jfpei)
c          print*, 'dphas,delr,sig2 =',
c     $         param(jfppha),param(jfpdr),param(jfpss2)
c          print*, 'theamp(28,jfeff)=', theamp(28,jfeff)
c          print*, 'qfeff(28,jfeff) =',  qfeff(28,jfeff)


cc          print*, chi_r(1), chi_i(1), chi_r(3), chi_i(3)

       end if
       return
       end
             





