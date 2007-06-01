       subroutine window(swin, dx1, dx2, xmin, xmax, xgrid, mpts, wa)
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
c purpose: create a window array for ffts 
c         (used to smooth out data and maintain peak separation).
c arguments:
c      swin:  window type (see notes below)             [in]
c      mpts:  dimension of wa                           [in]
c      dx1:   window parameters (see notes below)       [in]
c      dx2:   window parameters (see notes below)       [in]
c      xmin:  window range (see notes below)            [in]
c      xmax:  window range (see notes below)            [in]
c      xgrid: array grid, used to evaluate wa           [in]
c      wa:    array containing window function         [out]
c
c    notes: 9 window functions are supported.  many windows rise from
c    0 at x1 to 1 at x2, stay at 1 until x3 and drop to 0 at x4.
c    x1,...,x4 depend on window _type_ (iwin) and parameters
c    (dx1,dx2,xmin,xmax).  the gaussian window extends over the whole
c    input range and never equal 0.  the array is on an even grid
c    beginning at zero: wa(i) = wa(x=(i-1)*xgrid).
c
c  windows types are ( if swin = " ",  iwin will be set to 0).
c   iwin (swin)
c    0 (han):  hanning window sills (default):
c        x1 = xmin - dx1/2 ,   x2 = xmin + dx1/2
c        x3 = xmax - dx2/2 ,   x4 = xmax + dx2/2
c        the hanning function goes as cos^2 and sin^2.
c    1 (fha): hanning window fraction:
c        x1 = xmin ,   x2 = xmin + dx1*(xmax-xmin)/2
c        x4 = xmax,    x3 = xmax - dx1*(xmax-xmin)/2
c        the function goes as cos^2 and sin^2. dx1 is the
c        hanning fraction: the fraction of the x range over
c        which the windop is not 1. (dx1 = 1 will
c        give a full hanning fraction, with x2 = x3)
c    2 (gau): gaussian window
c        window(x) = exp( -dx1*(x - dx2)**2 )
c    3 (kai): Kaiser-Bessel window:
c       x1 = xmin ,   x4 = xmax,    x2,x3 not used
c       this function is similar to a Gaussian and goes to 0 at x1
c       and x4 for kbe = 5.44. Sometimes you will get a better resolution
c       in r-space for kbe = 2.72 (when the function isn't zero at 
c       x1 and x4. See the articel 'Digital Filter' by J.F. Kaiser in
c       'System Analysis by Digital Computers' edited by F.F. Kuo
c       and J.F. Kaiser, (New York; Wiley) 1966
c    4 (par): parzen window:
c        x1 = xmin - dx1/2 ,   x2 = xmin + dx1/2
c        x3 = xmax - dx2/2 ,   x4 = xmax + dx2/2
c        the window is linear between x1 and x2 and x3 and x4
c    5 (wel): welch window:
c        x1 = xmin - dx1/2 ,   x2 = xmin + dx1/2
c        x3 = xmax - dx2/2 ,   x4 = xmax + dx2/2
c        the window is parabolic between x1 and x2 and x3 and x4.
c    6 (sin): sine window:
c        x1 = xmin - dx1 ,   x4 = xmin + dx1
c        x2 and x3 =not used
c        this function is a sine that goes to 0 at x1 and x4
c        and is applied over the entire window range
c
c  for more information, see documentation for ifeffit
c
       implicit none
       integer mpts, iw, i, istrln
       character*(*) swin, s*32
       double precision   wa(mpts), halfpi, zero, one, half, eps
       double precision  x, x1, x2, x3, x4, xmin,xmax, xgrid, dx1, dx2
       double precision del1, del2, del12, del22
       double precision bessi0, bki0, bkav, bkde, bkde2, bkx, bkxx, bkom
       external bessi0, istrln
       parameter (halfpi= 1.570796326795d0, eps= 1.4d-5)
       parameter ( zero=0.d0, one=1.d0, half= 0.5d0) 
c determine window type
       s  = swin
       call triml(s)
       call lower(s)
       i  = istrln(s)
       iw = 0
       if     (s(1:3) .eq. 'fha') then
          iw = 1
       elseif (s(1:3) .eq. 'gau') then
          iw = 2
       elseif (s(1:3) .eq. 'kai') then
          iw = 3
       elseif (s(1:3) .eq. 'par') then
          iw = 4
       elseif (s(1:3) .eq. 'wel') then
          iw = 5
       elseif (s(1:3) .eq. 'sin') then
          iw = 6
       endif
c
       del1 = dx1
       del12= dx1 * half
       del2 = dx2
       del22= dx1 * half
       x1 = xmin
       x2 = 0
       x3 = 0
       x4 = xmax
c  set x1..x4 based on window type
c   hanning sills, parzen, and welch:
       x1 = xmin - del12
       x2 = xmin + del12  + (eps * xgrid) 
       x3 = xmax - del22  - (eps * xgrid)
       x4 = xmax + del22
cc       print*, 'U: iw, x1,x2,x3,x4',  iw, x1,x2,x3,x4
c   hanning fraction
       if (iw.eq.1) then
cc          print*, 'U: iw, x1,x2,x3,x4',  iw, x1,x2,x3,x4
          if (del12.lt.zero)  del12 = zero
          if (del12.gt.half)  del12 = half
          x2 = x1 + eps * xgrid + del12*(xmax-xmin)
          x3 = x4 - eps * xgrid - del12*(xmax-xmin) 
cc          print*, 'E: del12, del22, xgrid,eps=',del12, del12, xgrid, eps
cc          print*, 'E: x1, x2, x3, x4  = ', x1, x2, x3, x4
c   gaussian:
       elseif (iw.eq.2) then
          del1 = max(del1, eps)
c   sine
       elseif (iw.eq.6)  then
          x1 = xmin - del1
          x4 = xmax + del2
       end if
cc       print*, ' window ', xmin,xmax,del1,del2
cc       print*, ' window ', x1, x2, x3, x4, iw
c 
c now make the window array
c    hanning (fraction or sills)
       if (iw.le.1) then
          do 10 i=1,mpts
             x = (i-1)*xgrid
             if ((x.ge.x1).and.(x.le.x2)) then
                wa(i) = sin(halfpi*(x-x1) / (x2-x1)) ** 2
             elseif ((x.ge.x3).and.(x.le.x4)) then
                wa(i) = cos(halfpi*(x-x3) / (x4-x3)) ** 2
             elseif ((x.lt.x3).and.(x.gt.x2)) then
                wa(i) = one
             else
                wa(i) = zero
             endif
 10       continue
c    gaussian
       else if (iw.eq.2) then
          do 20 i = 1, mpts
             wa(i) =  exp( -(((i-1)*xgrid - del2)**2)/(2*del1*del1))
 20       continue
c     Kaiser-Bessel window
       elseif (iw.eq.3) then
          bki0  = bessi0(del1)
          bkav  = (x4+x1) * half
          bkde  = (x4-x1) * half 
          bkde2 = bkde * bkde
          bkom  = del1 / bkde
          do 30 i = 1, mpts
             wa(i) = zero
             x     = (i-1)*xgrid
             bkx   = x - bkav
             bkxx  = bkde2 - bkx*bkx
             if (bkxx.gt.0) then
                wa(i) = bessi0( bkom * sqrt(bkxx) ) / bki0
             endif
 30       continue 
c    parzen
       elseif (iw.eq.4) then
          do 40 i=1,mpts
             x = (i-1)*xgrid
             if ((x.ge.x1).and.(x.le.x2)) then
                wa(i) =  (x-x1) / (x2 - x1)
             elseif ((x.ge.x3).and.(x.le.x4)) then
                wa(i) = one - (x-x3) / (x4-x3)
             elseif ((x.lt.x3).and.(x.gt.x2)) then
                wa(i) = one
             else
                wa(i) = zero
             endif
 40       continue
c    welch
       elseif (iw.eq.5) then
          do 50 i=1, mpts
             x = (i-1)*xgrid
             if ((x.ge.x1).and.(x.le.x2)) then
                wa(i) = one - ((x-x2) / (x2-x1)) ** 2
             elseif ((x.ge.x3).and.(x.le.x4)) then
                wa(i) = one - ((x-x3) / (x4-x3)) ** 2
             elseif ((x.lt.x3).and.(x.gt.x2)) then
                wa(i) = one
             else
                wa(i) = zero
             endif
 50       continue
c    sine
       elseif (iw.eq.6) then
          do 60 i = 1, mpts
             x = (i-1)*xgrid
             if ((x.ge.x1).and.(x.le.x4))
     $            wa(i) = sin( 2* halfpi*(x4-x) / (x4-x1))
 60       continue
c    gaussian#2
       elseif (iw.eq.7) then
          do 70 i = 1, mpts
             x = (i-1)*xgrid
             wa(i) =  exp( -(del1 * (x - del2)**2 ))
 70       continue
       end if
       return
c end subroutine window
       end

