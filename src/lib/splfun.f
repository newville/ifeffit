       subroutine splfun(mf,nx,xv,ffit,iend)
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
c purpose:  evaluate spline function for lmdif1 minimization a la autobk
c
c arguments:
c      mf       number of points in ffit                [in]
c      nx       number of points in xv                  [in]
c      xv       vector of variables                     [in]
c      ffit     output vector to be minimized           [out]
c      iend     information tag                         [out]
c
c notes:
c   1. see subroutine lmdif1
c   1. see subroutine spline 
c   2. this evaluates the difference between the fft of theory and 
c      post-background-subtraction chi data for changing b-spline
c      coefficients, e0, and possibly multi-electron stuff.
c   3. based on/related to suroutine autnls from autobk
c
c see documentation and code comments for more details
c
c requires  include files:  consts.h, spline.h, fft.h
c           qintrp, lintrp, fitfft, bvalue, sumsqr
c
       implicit none
       include 'consts.h'
       include 'spline.h'
       include 'fft.h'
c
c local variables
       integer   nx, mf, iend, nvtmp, i, nrpts, ifit, j
       integer   nqmin, nqmax, nmaxx, ipos, nofx
       double precision   small, half, widmin
       parameter (half = 0.5d0, small = 1.d-10, widmin = 0.5d0)
       double precision xv(nx), ffit(mf), qtmp(maxpts), tmpfit(maxpts)
       double precision chiq(maxpts), qtt, e0t, bvalue
       double precision resid, tresid, sumsqr, t, fclamp
       external  bvalue, sumsqr,nofx
       save
c
       if (nx.ne.nautbk) iend = 1
       if (theory.and.(mf.ne.nr1st)) iend = 2
       if (.not.theory.and.(mf.ne.nrbkg))  iend = 3
       ifit   = 0
       nvtmp  = nx
       e0t    = e0
cc       print*, 'SPLFUN ', mf, nrbkg, nx, nsplin
cc       print*, xv
c
c  unwrap list of possible variables:
c     the last on the list from autnls is the first off
       if (theory.and.eevary) then
          thebkg = xv(nvtmp)
          de0    = xv(nvtmp)
          nvtmp  = nvtmp - 2
       end if
       if (nvtmp.ne.nsplin) iend = 3
c
c  e0 shift to temporary q-array for interpolation, evaluate the spline 
c  and calculate chi(E).
c         if normalizing by the functional form of the 
c        spline, make sure you're not dividing by zero.  
       e0t   = e0 + de0
cc       print*, ' splfun:  ', mf, e0, e0t, de0, nxmu
       do 200 i = 1, nxmu
          qtmp(i)   =  sqrt(etok * abs(endat(i)-e0t) )
          if (endat(i).lt.e0t)   qtmp(i) = - qtmp(i)
          if ( (endat(i).le.emax).and.(endat(i).ge.emin) ) then
             spldat(i)=bvalue(eknot,xv,nsplin,korder,endat(i),0)
             chie(i)   = ( xmudat(i) - spldat(i) ) / step
             if (funnrm)
     $            chie(i) = ( xmudat(i) /max(spldat(i), small)) - one
          else
             chie(i)   = zero
          end if
 200   continue
c  get qmax and qmin
       nqmin = int( sqrt( etok* abs(emin - e0t) ) / qgrid )
       nqmax = int( sqrt( etok* abs(emax - e0t) ) / qgrid )
       if ((emin.lt.e0t).or.(nqmin.lt.1))     nqmin = 1
c  interpolate chiq to q grid, evaluate data chi(k) [chiq]
       nmaxx  = min(maxpts, nqmax + 20)
       ipos   = 1
       call grid_interp(qtmp, chie, nxmu, zero, qgrid, nmaxx, chiq)
c
c get real and imaginary parts of the fft of chiq, put them in tmpfit
       call fitfft(chiq, maxpts, maxfft, wfftc, qgrid,
     $      splwin, splqw, spldat, one, 1, zero, r1st,
     $      nrpts, tmpfit)
c
       if (theory.and.(.not.thefix))
     $      thebkg = sumsqr(tmpfit(nrbkg),nr1st) / thessq
       do 550 i = 1, nrbkg
          ffit(i) = tmpfit(i) - splfit(i) * thebkg
 550   continue
       ifit = nrbkg
       if (theory.and.eevary) then
          do 560 i = nrbkg+1, nr1st
             ffit(i) = (tmpfit(i) - splfit(i)*thebkg) / 10.0
 560      continue 
          ifit = nr1st
       endif
c
c clamp first 4 chie() data points towards zero
       tresid = sumsqr(ffit,ifit)
cc       print*, 'end of splfun ' ,  tresid
       if (clamp(1)) then
          j = max(1,nofx(emin,endat,nxmu))
c
c scale clamp strength to residual, so that clamp strength is
c roughly 'percent importance' in fit.
c note '1+int(tresid)' here. this is to assure the scale is
c not zero, but also should make the scale factor settle down
c to a single number so that it does not affect the fitting
c time too much.
          fclamp = sclamp(1)*(1+int(tresid*100.d0)/nrbkg)/nclamp
          do 620 i = 1, nclamp
             ffit(i+ifit) = chie(i+j-1) * fclamp
cc             print*, i, ffit(i+ifit)
 620      continue 
          ifit = ifit + nclamp
       endif
c clamp last 4 chie() data points towards zero
       if (clamp(2)) then
          j = min(nxmu,max(1,nofx(emax,endat,nxmu)))
          fclamp = sclamp(2)*(1+int(tresid*100.d0)/nrbkg)/nclamp
c          print*, ' splfun clamp 2 at j=',j, endat(j),
c     $         emax, sclamp(2), fclamp
          do 630 i = 1, nclamp
             ffit(i+ifit) = chie(j + 1 - i) * fclamp
 630      continue 
          ifit = ifit + nclamp
       endif
cc       t= sumsqr(ffit,ifit)
cc       print*, 'end of splfun ' ,  tresid, t, ifit, nrbkg
       return
c end subroutine splfun
       end
