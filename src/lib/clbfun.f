       subroutine clbfun(mf,nx,xv,fv,iend)
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
c purpose:  minimization function for cromer-libermann bkg
c
c arguments:
c      mf       number of points in ffit                [in]
c      nx       number of points in xv                  [in]
c      xv       vector of variables                     [in]
c      ffit     output vector to be minimized           [out]
c      iend     information tag                         [out]
c
c notes:
c   heavily overrides values in spldat
c
c see documentation and code comments for more details
c
c requires  include files:  consts.h, spline.h

       implicit none
       include 'consts.h'
       include 'spline.h'
c
c local variables
       integer   nx, mf, iend, nvtmp, i, nrpts
       integer   nqmin, nqmax, nmaxx, ipos
       double precision st, a2, a1, a0 , ex
       double precision xv(nx), fv(mf), mod
       save
c
       a0 = xv(1)
       a1 = xv(2)
       a2 = xv(3)
       st = xv(4)
       do 100 i = 1, mf
          ex   = endat(i)
          mod  = a0 + ex * (a1 + a2 * ex) + spldat(i) * st
          fv(i) = (mod - xmudat(i)) * splfit(i)
 100   continue 
       return
c end subroutine clbfun
       end



