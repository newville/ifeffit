       subroutine conv_lor(gamma, mpts, x, y, step, yout)
c
c  broaden an array with a lorentzian of width gamma
c  typically used for broadening an energy-dependent array
c 
c  arguments:
c    gamma   lorenztian width (in eV)                [in]
c    mpts    size of x,y arrays                      [in]
c    x       array of x (energy)  values             [in]
c    y       array of y (ordinate values to broaden  [in]
c    step    step size (can be zero, see note)       [in]
c    yout    broadened array y(x)                    [out]
c
c  note:
c    for the convolution to work properly, the y(x) data 
c    needs to be on an even x-grid.  this is done here, so 
c    that input data does not need to be on an even grid. 
c  
c    if step is non-zero, that will be used as the grid spacing
c    if step is zero, the grid spacing will be found from the
c    data itself, as the smallest spacing in the input x array.
c    
c//////////////////////////////////////////////////////////////////////
c Copyright (c) 1999--2001 Matthew Newville, The University of Chicago
c
c Permission to use and redistribute the source code or binary forms of
c this software and its documentation, with or without modification is
c hereby granted provided that the above notice of copyright, these
c terms of use, and the disclaimer of warranty below appear in the
c source code and documentation, and that none of the names of The
c University of Chicago, or the authors appear in advertising or 
c endorsement of works derived from this software without specific 
c prior written permission from all parties.
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
       include 'maxpts.h'
       integer npts, i, j, mpts, ipos, nx1
       double precision x(*), y(*), yout(*), step, st, xt
       double precision gamma, sum, factr, zero,  small
       double precision tmpy, dx, gami, ytmp(maxpts)
       double precision xtmp(maxpts), tmp(maxpts)
       parameter  (zero = 0.d0, small = 1.d-9)

       st   = step
       npts = min(maxpts,mpts)
       if (npts.le.2)  return
       gami = 4.d0/(gamma*gamma)
c
c if step is too small (ie, zero) determine it from data
       if (st.le.small)  then
	  st = abs(x(2) - x(1))
          do 100 i = 3, npts
	    xt = abs(x(i) - x(i-1))
            if (xt.ge.small) st = min(st,xt)
 100     continue
       end if
c make sure step size / npts is OK....
 105   continue 
       nx1   = 1 + int( (x(npts) - x(1) + small)  / st)
       if (nx1.gt.maxpts) then
          st = st + st
          goto 105
       endif

c linearly interpolate onto even x grid
       ipos  = 1
       do 200 i = 1, nx1
          xtmp(i) = (i-1) * st + x(1)
          call lintrp(x, y, npts, xtmp(i), ipos, ytmp(i))
 200   continue 
c
c convolve
       do 500 i = 1, nx1
          sum  = zero
          tmpy = zero
          do 400 j = 1, nx1
             dx    = xtmp(j) - xtmp(i)
             factr = 1 / ( 1 + dx * dx  * gami)
             sum   = sum  + factr
             tmpy  = tmpy + ytmp(j) *  factr
 400      continue 
          tmp(i) = tmpy / max(small, sum)
 500   continue 
c
c put result back on input x-array
       ipos = 0
       do 700 i = 1, npts
          call lintrp(xtmp, tmp, nx1, x(i), ipos, yout(i))
 700   continue 
c
       return
       end

       subroutine conv_gau(gamma, mpts, x, y, step, yout)
c
c  broaden an array with a gaussian of width gamma
c  typically used for broadening an energy-dependent array
c 
c  arguments:
c    gamma   lorenztian width (in eV)                [in]
c    mpts    size of x,y arrays                      [in]
c    x       array of x (energy)  values             [in]
c    y       array of y (ordinate values to broaden  [in]
c    step    step size (can be zero, see note)       [in]
c    yout    broadened array y(x)                    [out]
c
c  note:
c    for the convolution to work properly, the y(x) data 
c    needs to be on an even x-grid.  this is done here, so 
c    that input data does not need to be on an even grid. 
c  
c    if step is non-zero, that will be used as the grid spacing
c    if step is zero, the grid spacing will be found from the
c    data itself, as the smallest spacing in the input x array.
c    
c//////////////////////////////////////////////////////////////////////
c Copyright (c) 1999--2001 Matthew Newville, The University of Chicago
c
c Permission to use and redistribute the source code or binary forms of
c this software and its documentation, with or without modification is
c hereby granted provided that the above notice of copyright, these
c terms of use, and the disclaimer of warranty below appear in the
c source code and documentation, and that none of the names of The
c University of Chicago, or the authors appear in advertising or 
c endorsement of works derived from this software without specific 
c prior written permission from all parties.
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
       include 'maxpts.h'
       integer npts, i, j, mpts, ipos, nx1
       double precision x(*), y(*), yout(*), step, st, xt
       double precision gamma, sum, factr, zero,  small
       double precision tmpy, dx, gami, ytmp(maxpts)
       double precision xtmp(maxpts), tmp(maxpts)
       parameter  (zero = 0.d0, small = 1.d-12)

       st   = step
       npts = min(maxpts,mpts)
       if (npts.le.2)  return
       gami = 1.d0/(2*gamma*gamma)
c
c if step is too small (ie, zero) determine it from data
       if (st.le.small)  then
	  st = abs(x(2) - x(1))
          do 100 i = 3, npts
	    xt = abs(x(i) - x(i-1))
            if (xt.ge.small) st = min(st,xt)
 100     continue
       end if
c make sure step size / npts is OK....
 105   continue 
       nx1   = 1 + int( (x(npts) - x(1) + small)  / st)
       if (nx1.gt.maxpts) then
          st = st + st
          goto 105
       endif
c
c linearly interpolate onto even x grid
       ipos  = 1
       do 200 i = 1, nx1
          xtmp(i) = (i-1) * st + x(1)
          call lintrp(x, y, npts, xtmp(i), ipos, ytmp(i))
 200   continue 
c
c convolve
       do 500 i = 1, nx1
          sum  = zero
          tmpy = zero
          do 400 j = 1, nx1
             dx    = xtmp(j) - xtmp(i)
             factr = exp(-gami*dx*dx)
             sum   = sum  + factr
             tmpy  = tmpy + ytmp(j) *  factr
 400      continue 
          tmp(i) = tmpy / max(small, sum)
 500   continue 
c
c put result back on input x-array
       ipos = 0
       do 700 i = 1, npts
          call lintrp(xtmp, tmp, nx1, x(i), ipos, yout(i))
 700   continue 
c
       return
       end
    
       subroutine do_gauss(x,mpts,cen,wid,out)
c
c  simple calculation of true gaussian function on input
c  array x
c    gauss(x,cen,wid) = 1/[wid*sqrt(2pi)] * exp[-(x-cen)^2/(2*wid^2)]
c 
c  arguments:
c    x       array of x values             [in]
c    mpts    size of x,y arrays            [in]
c    cen     centroid of gaussian          [in]
c    wid   width of gaussian             [in]
c    out     array of output values        [out]
c    
c//////////////////////////////////////////////////////////////////////
c Copyright (c) 1999--2001 Matthew Newville, The University of Chicago
c
c Permission to use and redistribute the source code or binary forms of
c this software and its documentation, with or without modification is
c hereby granted provided that the above notice of copyright, these
c terms of use, and the disclaimer of warranty below appear in the
c source code and documentation, and that none of the names of The
c University of Chicago, or the authors appear in advertising or 
c endorsement of works derived from this software without specific 
c prior written permission from all parties.
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
       include 'maxpts.h'
       integer npts, i, j, mpts
       double precision x(*), out(*), cen, wid
       double precision one,small,scale, escal, s2pi
       parameter (one  = 1.d0, small = 1.d-12)
c s2pi = 1 / sqrt(2*pi)
       parameter (s2pi = 0.3989422804014327d0)

c
       wid   =  max(small, wid)
       scale =  s2pi/ wid 
       escal = -one / (2 * wid * wid)
       do 100 i = 1, mpts
          out(i) = scale * exp(escal*(x(i) - cen)*(x(i) - cen))
 100   continue 
c
       return
       end
     
       subroutine do_loren(x,mpts,cen,wid,out)
c
c  simple calculation of true lorenztian function on input
c  array x
c    loren(x,cen,wid) = wid/(2*pi) * [ (x-cen)^2 + (wid/2)^2]^(-1)
c 
c  arguments:
c    x       array of x values             [in]
c    mpts    size of x,y arrays            [in]
c    cen     centroid of lorentzian        [in]
c    wid     width of lorentzian           [in]
c    out     array of output values        [out]
c    
c//////////////////////////////////////////////////////////////////////
c Copyright (c) 1999--2001 Matthew Newville, The University of Chicago
c
c Permission to use and redistribute the source code or binary forms of
c this software and its documentation, with or without modification is
c hereby granted provided that the above notice of copyright, these
c terms of use, and the disclaimer of warranty below appear in the
c source code and documentation, and that none of the names of The
c University of Chicago, or the authors appear in advertising or 
c endorsement of works derived from this software without specific 
c prior written permission from all parties.
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
       include 'maxpts.h'
       integer npts, i, j, mpts
       double precision x(*), out(*), cen, wid
       double precision four, small,scale, wid2, pi, pi2
       parameter (four = 4.d0, small = 1.d-12)
       parameter (pi   = 3.141592653589793d0)
       parameter (pi2  = 2.d0 * pi)
c
c    loren(x,cen,wid) = wid/(2*pi) * [ (x-cen)^2 + (wid/2)^2]^(-1)

       wid   = max(small, wid)
       scale = wid / pi2
       wid2  = wid * wid / four
       do 100 i = 1, mpts
          out(i) = scale / ((x(i)-cen)*(x(i)-cen) + wid2)
 100   continue 
c
       return
       end
     
       subroutine do_pvoight(x,mpts,cen,fwhm,frac,out)
c
c  simple calculation of pseudo-voight as weighted sum
c  of lorenztian and gaussian functions
c   voight(x,cen,wid,frac) = frac *loren(x,cen,wid) +
c                           (1-frac)*gauss(x,cen,wid)
c 
c  arguments:
c    x       array of x values             [in]
c    mpts    size of x,y arrays            [in]
c    cen     centroid of loren/gauss       [in]
c    wid     width of loren/gauss          [in]
c    frac    fraction for lorentzian       [in]
c    out     array of output values        [out]
c    
c//////////////////////////////////////////////////////////////////////
c Copyright (c) 1999--2001 Matthew Newville, The University of Chicago
c
c Permission to use and redistribute the source code or binary forms of
c this software and its documentation, with or without modification is
c hereby granted provided that the above notice of copyright, these
c terms of use, and the disclaimer of warranty below appear in the
c source code and documentation, and that none of the names of The
c University of Chicago, or the authors appear in advertising or 
c endorsement of works derived from this software without specific 
c prior written permission from all parties.
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
       include 'maxpts.h'
       integer npts, i, j, mpts
       double precision x(*), out(*), cen, fwhm, frac
       double precision  tmp(maxpts), sig, conv
c   conv = 1/ (2 * sqrt(2*log(2)))
       parameter (conv = 0.424660900144d0)
       
       sig = fwhm * conv
       call do_loren(x,mpts,cen,fwhm,out)
       call do_gauss(x,mpts,cen,sig,tmp)
       do 100 i = 1, mpts
          out(i) = frac*out(i) + (1-frac)*tmp(i)
 100   continue 
c
       return
       end
     

