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
       integer function  nofxa(x,array,npts)
c
c   return index in array with value closest to scalar x.
c   arguments
c   x      value to find in array  
c   array  double precision array (not necessarily monotonically increasing)
c   npts   number of points in array
c
       implicit none
       integer npts,  i, it
       double precision array(npts), x, xit, xmin
c
c linear search
       xmin = abs(array(1) - x)
       it   = 1
       do 10 i = 2, npts
          xit = abs(array(i) - x)
          if (xit .lt. xmin) then
             xmin = xit
             it   = i
          endif
 10    continue
       nofxa = it
       return
c end function nofxa
       end
       integer function  nofx(x,array,npts)
c
c   return index in array with value closest to scalar x.
c   arguments
c   x      value to find in array  
c   array  double precision array (monotonically increasing)
c   npts   number of points in array
c
       implicit none
       integer npts, imin, imax, inc, it
       double precision array(npts), x, xit, xave
c
c hunt by bisection
       imin = 1
       imax = npts
       inc = ( imax - imin ) / 2
 10    continue
       it  = imin + inc
       xit = array(it)
       if ( x .lt. xit ) then
          imax = it
       else if ( x .gt. xit ) then
          imin = it
       else
          nofx = it
          return
       endif
       inc = ( imax - imin ) / 2
       if ( inc .gt. 0 ) go to 10
c x is between imin and imin+1
       xave = ( array(imin) + array(imin+1) ) / 2
       if ( x .lt. xave ) then
          nofx = imin
       else
          nofx = imin + 1
       endif
       return
c end function nofx
       end
       integer function  nofxsp(x,array,npts)
c
c   return index in array with value closest to scalar x.
c   arguments
c   x      value to find in array  
c   array  single precision array (monotonically increasing)
c   npts   number of points in array
c
       integer npts, imin, imax, inc, it
       real array(npts), x
c
c hunt by bisection
       imin = 1
       imax = npts
       inc = ( imax - imin ) / 2
 10    continue
       it  = imin + inc
       xit = array(it)
       if ( x .lt. xit ) then
          imax = it
       else if ( x .gt. xit ) then
          imin = it
       else
          nofxsp = it
          return
       endif
       inc = ( imax - imin ) / 2
       if ( inc .gt. 0 ) go to 10
c bisection
       xave = ( array(imin) + array(imin+1) ) / 2.
       if ( x .lt. xave ) then
          nofxsp = imin
       else
          nofxsp = imin + 1
      endif
       return
c end function nofxsp
       end
       subroutine hunt(xar, npts, xin, jlo)
c
c   return jlo=lower-bound index of a value xin in array xar(n)
c   such that xar(jlo) <= xin < xar(jlo+1). 
c arguments:
c   xar   monotonically increasing array     [in]
c   npts  length of xar                      [in]
c   xin   value to hunt for                  [in]
c   jlo   initial guess /  output index      [in/out]
c
       implicit none
       integer npts, jlo, jhi, inc, jm
       double precision  xar(npts), xin
       logical  dohunt

c  first, decide if we really need to do a hunt at all
c  or if the initial guess (jlo) was good enough: it often is!
cc       print*, '-> hunt ', npts, xin,jlo, xar(1), xar(2), xar(3)
       dohunt = .true.
       jlo    = min(npts-1,max(1,jlo))
       if ((xin.gt.xar(jlo)) .and. (xin.lt.xar(jlo+1))) then
          dohunt = .false.
       elseif (xin.le.xar(1)) then
          jlo     = 1
          dohunt = .false.
       elseif (xin.ge.xar(npts)) then
          jlo    = npts - 1
          dohunt = .false.
c
c check next interval -- often the right choice if the current
cc interval was not.
       elseif (jlo.le.npts-2) then
          if ((xin.gt.xar(jlo+1)) .and. (xin.le.xar(jlo+2))) then
             jlo    = jlo + 1
             dohunt = .false.
          end if
c      
       end if
c hunt the old-fashioned way:
       if (dohunt) then
cc          print*, 'hunt: real hunt ', jlo, xin, xar(jlo),xar(jlo+1)
cc     $         ,xar(jlo+2),xar(jlo+3)
          if (jlo.le.0.or.jlo.gt.npts) then
c the input jlo is not useful -- do bisection
             jlo = 0
             jhi = npts+1
             go to 30
          endif
          inc = 1
c  look ever further away to bracket value
c    hunting up from current guess
          if (xin.ge.xar(jlo)) then
 10          continue 
             jhi=jlo+inc
             if (jhi.gt.npts) then
                jhi=npts+1
             elseif (xin.ge.xar(jhi)) then
                jlo=jhi
                inc=inc+inc
                go to 10
             endif
          else
c    hunting down from current guess
             jhi=jlo
 20          continue 
             jlo=jhi-inc
             if (jlo.lt.1) then
                jlo=0
             elseif (xin.lt.xar(jlo)) then
                jhi=jlo
                inc=inc+inc
                go to 20
             endif
          endif
c   now use bisection to reduce
c   the bracket interval to 1
 30       continue
          if (jhi.ne.(jlo+1)) then
             jm = (jhi + jlo) / 2
             if (xin.gt.xar(jm)) then
                jlo=jm
             else
                jhi=jm
             endif
             go to 30
          end if
       end if
       jlo    = min(npts-1,max(1,jlo))
       return
c end subroutine hunt
       end
       subroutine xterp(xnew, nxnew, y, ny, x, nx, iterp, ierr)
c
c  interpolate yold(xold) to ynew(xnew)  using interpolation
c  scheme defined by iterp
c  arguments
c     xnew   xnew array on input         [in/out]
c            ynew array on output 
c     y      yold array                  [in]
c     x      xold array                  [in]
c     iterp  interpolation method
c
c  copyright (c) 1998  matt newville
       implicit none
       include 'maxpts.h'
       integer   nx, ny, nxnew, i, ierr, ip, iterp
       double precision x(*), y(*), xnew(*)
       double precision tmp(maxpts), coefs(maxpts)
       ierr = 0
       ip   = 1
c
ccc       print*, ' XTERP: ', iterp
       ny   = min(nx,ny)
       if (iterp .eq. 0) then 
          do 20 i = 1, nxnew
             call lintrp(x, y, ny, xnew(i), ip, tmp(i))
 20       continue 
       elseif (iterp .eq. 1) then
          do 30 i = 1, nxnew
             call qintrp(x, y, ny, xnew(i), ip, tmp(i))
 30       continue 
       elseif (iterp .eq. 2) then
          call splcoefs(x, y, ny, coefs, tmp)
          do 80 i = 1, nxnew
             call splint(x, y, coefs, ny, xnew(i), ip, tmp(i))
 80       continue 
       end if
c
       do 100 i = 1, nxnew
          xnew(i) = tmp(i)
 100   continue 
       return
c end subroutine xterp
       end


       subroutine splcoefs(x, y, npts, c, t)
c
c calculate simple (natural) cubic spline coefficients
c given a pair of arrays x, y
c
c c:  output array
c t:  temporary work array
       implicit none
       integer    npts, ip, i
       double precision  x(*), y(*), c(*), t(*)
       double precision  tiny, zero, xin, yout, one
       parameter (zero = 0.d0, one = 1.d0)
       double precision  s, p, dxp, dxm, dx2
       
cc       print*, '>> splcoefs '
       c(1)    = zero
       t(1)    = zero
       c(npts) = zero
       do 20 i = 2, npts - 1
          dx2  = one / ( x(i+1) - x(i-1) )
          dxp  = one / ( x(i+1) - x(i)   )
          dxm  = one / ( x(i)   - x(i-1) )
          s    = dx2 * ( x(i)   - x(i-1) )
          p    = one / (2 + s * c(i-1))
          c(i) = (s - one) * p
          t(i) = p * 
     $     (6*dx2*((y(i+1)-y(i))*dxp - (y(i)-y(i-1))*dxm) - s*t(i-1))
 20    continue 
       do 30 i = npts-1,1, -1
          c(i) = c(i)*c(i+1) + t(i)
 30    continue 
       return
       end

       subroutine splint(x, y, c, npts, xin, ip, yout)
c
c simple natural cubic spline interpolation using
c array of coefficients for splcoefs
c
       implicit none
       integer    npts, ip
       double precision x(*), y(*), c(*)
       double precision xin, yout, sixth, dx, dxi, a,b
       parameter (sixth = 1.d0 / 6.d0)

c  make sure ip is in range
c  find ip such that   x(ip) <= xin <= x(ip+1)
       call hunt(x, npts, xin, ip)
       dx  =  x(ip+1) - x(ip)
       dxi = 1.d0 / dx
       a   = (x(ip+1) - xin  ) * dxi
       b   = (xin     - x(ip)) * dxi
       yout= a*y(ip) + b*y(ip+1)  +  dx*dx* sixth * 
     $      (a*(a*a-1)*c(ip) + b*(b*b-1)*c(ip+1)) 
       
       return
       end
       subroutine lintrp(x, y, npts, xin, ip, yout)
c
c    linear interpolation for use in loops where xin increases 
c    steadily through the monotonically increasing array x. 
c  arguments:
c     x      array of ordinate values                   [in]
c     y      array of abscissa values                   [in]
c     npts   length of arrays x and y                   [in]
c     xin    value of x at which to interpolate         [in]
c     ip     index such that x(ip) <= xin <= x(ip+1)    [in/out]
c     y      interpolated abscissa at xin               [out]
c  note: this routine is called extremely often 
c        -- anything to improve efficiency should be done
       implicit none
       integer    npts, ip
       double precision   x(*), y(*), tiny, xin, yout
       parameter  (tiny = 1.d-9)
c  find ip such that   x(ip) <= xin < x(ip+1)
       call hunt(x, npts, xin, ip)
       yout = y(ip) 
       if ((x(ip+1)-x(ip)) .gt. tiny)  yout = yout +
     $     (y(ip+1)-y(ip)) * (xin-x(ip)) / (x(ip+1)-x(ip))
       return
c  end subroutine lintrp
       end
       subroutine qintrp(x, y, npts, xin, ip, yout)
c
c     this does a crude quadratic interpolation for repeated loops 
c     where xin is increasing steadily through the values in x. 
c   inputs:
c     x      array of ordinate values
c     y      array of abscissa values
c     npts   length of arrays x and y
c     xin    value of x at which to interpolate 
c     ip     guess of index in x array to use 
c  outputs: 
c     ip     index in x array used in interpolation
c     yout    interpolated abscissa at xin
c----------------------------------------------------------------
       implicit none
       integer    npts, ip, i1, i2, i3a, i3b, imin, imax
       double precision  x(npts), y(npts), tiny, xin, yout
       double precision dxi3a, dxi3b, dx12, dx13b, dx23a, dx23b
       double precision youta, youtb, dxi1, dxi2, dx13a
       parameter  (tiny = 1.d-9)

c  find ip such that   x(ip) <= xin <= x(ip+1)
c   most likely candidate is the current value of ip, or ip+1
c   otherwise use routine hunt to find ip

c  find ip such that   x(ip) <= xin < x(ip+1)
       call hunt(x, npts, xin, ip)
       yout  = y(ip)
c
       if ((x(ip+1)-x(ip)).gt.tiny) then
c find two closest x values and the two further neighbors
          i1 = ip
          i2 = ip + 1
          if (xin.lt.x(ip))    i2 = ip - 1
          i3a = max(i1,i2) + 1
          i3b = min(i1,i2) - 1
          imin = min(i1,i2,i3a,i3b)
          imax = max(i1,i2,i3a,i3b)
          if ((imin.gt.3).and.(imax.lt.npts-2)) then
c construct differences
             dxi1  =  xin   - x(i1)
             dxi2  =  xin   - x(i2)
             dxi3a =  xin   - x(i3a)
             dxi3b =  xin   - x(i3b)
             dx12  =  x(i1) - x(i2)
             dx13a =  x(i1) - x(i3a)
             dx13b =  x(i1) - x(i3b)
             dx23a =  x(i2) - x(i3a)
             dx23b =  x(i2) - x(i3b)
             youta = dxi2 * dxi3a * y(i1)  / ( dx12  * dx13a )
     $             - dxi1 * dxi3a * y(i2)  / ( dx12  * dx23a )
     $             + dxi1 * dxi2  * y(i3a) / ( dx13a * dx23a )
             youtb = dxi2 * dxi3b * y(i1)  / ( dx12  * dx13b )
     $             - dxi1 * dxi3b * y(i2)  / ( dx12  * dx23b )
     $             + dxi1 * dxi2  * y(i3b) / ( dx13b * dx23b )
             yout  = (youta * dxi3b - youtb * dxi3a)/(x(i3a) - x(i3b))
          else
             call lintrp(x, y, npts, xin, ip, yout)
          end if
       end if
       return
c  end subroutine qintrp
       end
       integer function iff_get_interp(s)
       character*(*) s, t*16
       integer  i, istrln
       external istrln
       t = s
       call triml(t)
       i = istrln(t)
       j = 2
       if (t(1:4) .eq. 'line')   j = 1
       if (t(1:4) .eq. 'quad')   j = 2
       if (t(1:5) .eq. 'cubic')  j = 3
       if (t(1:6) .eq. 'spline') j = 3
       iff_get_interp = j
       end

      double precision function determ(array,nord,nrows)
c
c  calculate determinate of a square matrix
c
c  arguments  (all strictly input): 
c     array   matrix to be analyzed
c     nord    order of matrix
c     nrows   first dimension of matrix in calling routine
c 
c  copyright (c) 1998  matt newville
c
c  base on bevington "data reduction and error analysis
c  for the physical sciences" pg 294
c
       implicit double precision (a-h,o-z) 
       integer nord, nrows,  i, j, k
       double precision array(nrows,nrows)
       logical      iszero
       determ = 1
       do 150 k=1,nord
c
          if (array(k,k).eq.0) then
             iszero = .true.
             do 120 j=k,nord
                if (array(k,j).ne.0) then 
                   iszero =.false.
                   do 100 i=k,nord
                      saved = array(i,j)
                      array(i,j) = array(i,k)
                      array(i,k) = saved
 100               continue 
                   determ = -determ
                end if
 120         continue
             if (iszero) then 
                determ = 0
                return
             end if
c
          end if
          determ = determ*array(k,k)
          if (k.lt.nord) then
             k1 = k+1
             do 140 i=k1,nord
                do 130 j=k1,nord
                   array(i,j) = array(i,j)-
     $                  array(i,k)*array(k,j)/array(k,k)
 130            continue 
 140         continue 
          end if
 150   continue
c end double precision function determ 
       end
       double precision function bessi0(x)
c
c zero-ordered modified Bessel function I_0(x) for real x
c from abramowitz and stegun p 378 
       double precision x, v, y, c
       double precision a1,a2,a3,a4,a5,a6
       double precision b1,b2,b3,b4,b5,b6,b7,b8,b9
       parameter(a1 = 3.5156229d0  , a2 = 3.0899424d0  )
       parameter(a3 = 1.2067492d0  , a4 = 0.2659732d0  )
       parameter(a5 = 0.360768d-1  , a6 = 0.45813d-2   )
       parameter(b1 = 0.39894228d0 , b2 = 0.1328592d-1 )
       parameter(b3 = 0.225319d-2  , b4 =-0.157565d-2  )
       parameter(b5 = 0.916281d-2  , b6 =-0.2057706d-1 )
       parameter(b7 = 0.2635537d-1 , b8 =-0.1647633d-1 )
       parameter(b9 = 0.392377d-2  ,  c = 3.75d0)
c
       v = abs(x)
       if(v.lt.c) then
          y=(x/c)**2
          bessi0= 1 + y*(a1+y*(a2+y*(a3+y*(a4+y*(a5+y*a6)))))
       else
          y=c/v
          bessi0=(exp(v)/sqrt(v)) *
     $    (b1+y*(b2+y*(b3+y*(b4+y*(b5+y*(b6+y*(b7+y*(b8+y*b9))))))))
       endif
      return
      end
       double precision  function sumsqr(array, narray)
c  returns sum of squares of an array with dimension narray
       double precision  array(*),  big, zero
       parameter( big = 1.d17, zero = 0d0)
       sumsqr  = zero
       do 50 i = 1, narray
          if (abs(array(i)).lt.big) then
             sumsqr = sumsqr + array(i)*array(i)
          else
             sumsqr = sumsqr + big*big
          end if
 50    continue
       return
c  end real function sumsqr
       end
      subroutine pijump (ph, old)
c
c     removes jumps of 2*pi in phases
c     ph = current value of phase (may be modified on output, but
c          only by multiples of 2*pi)
c     old = previous value of phase
       integer isave, jump, i
       double precision xph(3), pi, twopi, old, xphmin, ph
       parameter (pi = 3.14159 26535 89793 23846 26433d0)
       parameter (twopi = 2 * pi)

       isave  = 1
       xph(1) = ph - old
       jump   = int( (abs(xph(1))+ pi) / twopi)
       xph(2) = xph(1) - jump*twopi
       xph(3) = xph(1) + jump*twopi
       
       xphmin = min (abs(xph(1)), abs(xph(2)), abs(xph(3)))
       do 10  i = 1, 3
          if (abs (xphmin - abs(xph(i))) .le. 1.d-2)  isave = i
 10    continue

       ph = old + xph(isave)
       
       return
c end subroutine pijump
       end
       
       subroutine polyft(xfit1,xfit2,xdata,ydata,ndata,nterms,aout)
c
c  get coefficients for polynomial fit :
c      ydata = aout(1) + aout(2)*xdata  + aout(3) *xdata^2 + ...
c  the fit is done between xdata = [xfit1, xfit2]
c
c  arguments:
c   xfit1   lower bound of fitting range       (single precision) (in)
c   xfit2   upper bound of fitting range       (single precision) (in)
c   xdata   array of abscissa values for data  (single precision) (in)
c   ydata   array of ordinate values for data  (single precision) (in)
c   ndata   length of data arrays                                 (in)
c   nterms  number of terms in polynomial                         (in)
c   aout    coefficients of fitted polynomial  (single precision) (out)
c
c  requires functions nofx and determ.
c  note that double and single precision are mixed here. 
c  most internal, working arrays use dp (as does routine determ)
c  
c
c  copyright (c) 1998  matt newville
c
c  see bevington pg 104 for details
c
       implicit none
       integer max, max2m1, ndata, nterms, i, j, l, k, n, ntemp
       integer nfit1, nfit2, nmax, nofx
       double precision xdata(ndata), ydata(ndata), aout(nterms)
       double precision zero, one, xi, yi, xterm, yterm, xfit1, xfit2
       parameter (max= 5, max2m1 = 2*max-1, zero = 0.d0,one=1.d0)
       double precision  sumx(max2m1), sumy(max)
       double precision  array(max,max), ain(max), delta, determ
       external          determ, nofx
c
c     initialize internal arrays
       nmax   = 2 * nterms - 1
       do 100 i=1, nmax
          sumx(i) = zero
 100   continue
       do 120 i = 1, nterms
          ain(i) = zero
          sumy(i) = zero
          do 110 j = 1,  nterms
             array(i,j) = zero       
 110      continue
 120   continue
c     
c     find points closest to endpoints of fitting range
       nfit1 = nofx(xfit1,xdata,ndata)
       nfit2 = nofx(xfit2,xdata,ndata)
       if (nfit1.gt.nfit2) then
          ntemp = nfit1
          nfit1 = nfit2
          nfit2 = ntemp
       end if
       if(nfit1.eq.nfit2) go to 300
c     
c     collect sums of data, sum of squares of data, etc.
       do 200 i = nfit1, nfit2 
          xi = xdata(i)
          yi = ydata(i)
          xterm = one
          do 180 n=1, nmax
             sumx(n) = sumx(n) + xterm
             xterm   = xterm * xi
 180      continue
          yterm = yi
          do 190 n=1,nterms
             sumy(n) = sumy(n) + yterm
             yterm   = yterm * xi
 190      continue 
 200   continue
c     
c     construct matrices and evaluate coefficients
       do 220 j=1,nterms
          do 210 k=1,nterms
             array(j,k) = sumx(j + k - 1)
 210      continue 
 220   continue 
c
c     take determinant, get coefficients  
       delta = determ(array,nterms,max)
       if (delta.ne.zero) then
          do 260 l=1,nterms
             do 250 j=1,nterms
                do 240 k=1,nterms
                   array(j,k) = sumx(j+k-1)
 240            continue
                array(j,l) = sumy(j)
 250         continue
             ain(l) = determ(array,nterms,max)/delta
 260      continue
       end if
c
c     convert coefficients to single precision, leave
 300   continue
       do 400 i = 1, nterms
          aout(i) = ain(i)
 400   continue
       return
c end  subroutine polyft
       end

       subroutine gaussj(a, n, ma, ierr)
c
c gauss-jordan elimination to invert a matrix.
c arguments:
c   a        matrix to invert / solution on output     [in/out]
c   n        number of elements in a to use            [in]
c               (i.e. that aren't zero) 
c   ma       dimension of a                            [in]
c   ierr     0 on success / 1  on error 
c notes:
c    if matrix cannot be inverted, a  contains garbage
c
c copyright (c) 1998 matt newville
c
       implicit none
       include 'consts.h'
       integer  n, ma, i, j,k,l,m, irow, icol, ierr
       integer  ipiv(mvarys), indrow(mvarys), indcol(mvarys)
       double precision a(ma,ma),  abig, tmp, piv
c
       ierr  = 1
       irow  = 0
       icol  = 0
c initialize pivot array
       do 30 i = 1, n
          ipiv(i) = 0
 30    continue
c
c  main loop over the columns to be reduced
       do 300 i = 1, n
          abig = zero
c linear search for a pivot element
          do 120 j = 1, n
             if (ipiv(j).ne.1) then
                do 100 k = 1, n
                   if (ipiv(k).eq.0) then
                      if ( abs(a(j,k)) .ge. abig) then
                         abig = abs(a(j,k))
                         irow = j
                         icol = k
                      endif
                   endif
 100            continue
             endif
 120      continue
          ipiv(icol) = ipiv(icol) + 1
c a pivot has been found
          if (irow.ne.icol) then
             do 160 l = 1, n
                tmp        = a(irow, l)
                a(irow, l) = a(icol, l)
                a(icol, l) = tmp
 160         continue
          endif
c divide the pivot row by the pivot element
          indrow(i) = irow
          indcol(i) = icol
          if (a(icol, icol).eq.zero) return
          piv          = one / a(icol, icol)
          a(icol,icol) = one
          do 200 l = 1, n
             a(icol, l) = a(icol, l) * piv
 200      continue
c reduce non-pviot rows
          do 250 m = 1, n
             if (m.ne.icol) then
                tmp        = a(m, icol)
                a(m,icol) = zero
                do 220 l = 1, n
                   a(m,l) = a(m,l) - a(icol,l) * tmp
 220            continue
             endif
 250      continue
 300   continue
c
c   unravel the solution: interchange column pairs
c   in the reverse order of the permutation 
       ierr = 0
       do 400 i = n, 1, -1
          if (indrow(i) .ne. indcol(i)) then
             do 350 j = 1, n
                tmp            = a(j,indrow(i))
                a(j,indrow(i)) = a(j,indcol(i))
                a(j,indcol(i)) = tmp
 350         continue
          endif
 400   continue
c
       return
c  end subroutine gaussj
       end
       double precision function rfact(xdata, theory, ndata)
c
c      compute an xafs reliability factor as a measure of the
c      goodness of fit between arrays for data and theory.
c input:
c    xdata   (real,imag) pairs for data   over fit range
c    theory  (real,imag) pairs for theory over fit range
c    ndata   number of data points to use
c output:
c
c            sum{ [re(xdata) - re(theory)]^2 + [im(xdata) - im(theory)]^2 }
c    rfact =  ------------------------------------------------------------
c            sum{ [re(xdata)]^2 + [im(xdata)]^2 }
c
c      copyright 1999 matt newville
c
       double precision  xdata(*), theory(*), ampl, small
       integer  ndata, i
       parameter(small = 1.d-08)
c initialize
       ampl   = 0
       rfact  = 0
c  construct sums of squares
       do 100 i = 1, ndata
          ampl  = ampl  +  xdata(i)**2 
          rfact = rfact + (xdata(i)  - theory(i))**2
 100   continue
       rfact =  rfact  / max(small, ampl)
       return
c end function rfact
       end


       subroutine kev2ev(e, ne)
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
c  test and (if needed) convert an energy array in KeV to eV 
       implicit none
       integer ne, i
       double precision e(ne), de
       
       if ((e(1).le.50).and.(e(ne).le.50)) then
          de = e(2) - e(1)
          do 300 i = 2, ne
             de = min(de, (e(i) - e(i-1)))
 300      continue 
          if (de .le. 0.01) then 
             do 310 i = 1, ne
                e(i) = e(i) * 1000
 310         continue 
          endif
       endif
       return
       end
c
       subroutine grid_interp(x, y, nx, tmin, dt, nout, yout)
c
c  convert x,y data on a non-uniform grid to be
c  on a uniform (defined by tmin, nt, and dt)
c 
c  at each t=tmin + (i-1)*dt (i: 1 to nout)
c    if 1 or more x value lies in the range (t-dt/2,t+dt/2)
c       all the available y points are averaged (constant weight)
c    if 0 x values lie in the range (t-dt/2,t+dt/2),
c       a quadratic interpolation of the closest upper and lower
c       (x,y) points is used.
c   inputs:
c     x      array of ordinate values
c     y      array of abscissa values
c     nx     length of arrays x and y
c     tmin   first x value of output y array
c     dt     grid spacing for gridded x values
c     nout   number of points to calculate, length of yout
c  outputs: 
c     yout   interpolated abscissa on supplied x grid
c  notes:
c     using lorenztian weighting with gamma=dt would seem
c     reasonable, but is commented-out for now...
c----------------------------------------------------------------
       implicit none
       integer    nx, nout, i, j, ilo, ihi, iqt,  npts, jjj
       double precision  x(*), y(*), yout(*), tmin, dt
       double precision x0, xlo, xhi, sum, weight, gam2
       double precision tiny,  onem, half, zero, one
       parameter  (tiny = 1.d-9, half=0.5d0,zero=0.d0)
       parameter  (one  = 1.d0,  onem = 1.d0 - tiny)

c  find ip such that   x(ip) <= xin <= x(ip+1)
c   most likely candidate is the current value of ip, or ip+1
c   otherwise use routine hunt to find ip
       ihi  = -1
       ilo  = -1
       iqt  =  0
       gam2 =  dt*dt

       do 100 i = 1, nout
          x0  = tmin + dt*(i-1)
          xlo = x0   - dt*half
          xhi = xlo  + dt*onem
c first point, or if for some other reason, 
c we haven't found a low endpoint, find it now
          ilo = ihi + 1
          if (ilo.le.0)  call hunt(x, nx, xlo, ilo)
c find high end of grid range
          call hunt(x, nx, xhi, ihi)
c
c if we have more than 1 point in this region, do a 
c weighted average of the points in the region
          if ((ihi-ilo).ge.1)  then
             sum     = zero
             yout(i) = zero
             weight  = one
             do 50 j = ilo, ihi 
cc                this would provide a lorenztian weighting....
cc                weight = one /(((x(j)-x0)*(x(j)-x0)) + gam2)
                sum    = sum     +      weight
                yout(i)= yout(i) + y(j)*weight
 50          continue 
             yout(i)   = yout(i) / max(tiny,sum)
          else 
c if we have no input data points in this region, 
c use quadratic interpolation of available data
             iqt = ilo
             call qintrp(x,y,nx,x0,iqt,yout(i))
             if (yout(i).ne.yout(i)) then 
                call warn(3,' bad data point in interpolation!')
cc                print*, ' Grid ', i, '  bad qintrp '
cc                print*, ' iqt = ', iqt
cc                do jjj  = -2,2 
cc                   print*, jjj+iqt, x(jjj+iqt), y(jjj+iqt)
cc                end do
             endif
          endif
 100   continue 
       return
c  end subroutine grid_interp
       end
c
       subroutine rebin_interp(xout, nxout, yin, nyin, xin, nxin)
c
c  similar to grid_interp above: converts input x,y data to 
c  provided output x array, using boxcar average:
c 
c  at each x=xout (i: 1 to nxout)
c    if 1 or more xin value lies in the range 
c          [(xout(i)+xout(i-1))/2:(x(i+1)+x(i))/2]
c       all the available y points are averaged (constant weight)
c    if 0 xin values lie in the range 
c          [(xout(i)+xout(i-1))/2:(x(i+1)+x(i))/2]
c       a linear interpolation of the closest upper and lower
c       (x,y) points is used.
c   inputs:
c     xin     array of input ordinate values
c     yin     array of input abscissa values
c     nxin    length of arrays xin and yin
c     xout    array of output ordinates
c     nxout   length of xout
c  outputs: 
c     xout    interpolated abscissa on supplied xout
c  notes:
c    - using lorenztian weighting with gamma=dt would seem
c      reasonable....
c    - weird call structure due to decod (see xterp above) 
c----------------------------------------------------------------
       implicit none
       include 'maxpts.h'
       integer    nxin, nxout, i, j, ilo, ihi, iqt,  npts, nyin
       double precision  xin(*), yin(*), xout(*), yout(maxpts)
       double precision x0, xlo, xhi, sum, weight, gam2
       double precision tiny,  onem, half, zero, one
       parameter  (tiny = 1.d-9, half=0.5d0,zero=0.d0)
       parameter  (one  = 1.d0,  onem = 1.d0 - tiny)

c  find ip such that   x(ip) <= xin <= x(ip+1)
c   most likely candidate is the current value of ip, or ip+1
c   otherwise use routine hunt to find ip
       ihi  = -1
       ilo  = -1
       iqt  =  0
       nxin = min(nxin,nyin)
       do 100 i = 1, nxout
          x0  = xout(i)
          xlo = x0
          xhi = x0
          if (i.gt.1)     xlo = (x0 + xout(i-1))*half
          if (i.lt.nxout) xhi = (x0 + xout(i+1)-tiny)*half

c first point, or if for some other reason, 
c we haven't found a low endpoint, find it now
          ilo = ihi + 1
          if (ilo.le.0)  call hunt(xin, nxin, xlo, ilo)
c find high end of grid range
          call hunt(xin, nxin, xhi, ihi)
c
c if we have more than 1 point in this region, do a 
c weighted average of the points in the region
cc          print*, ' ' , i, x0, xlo, xhi, ilo, ihi, xin(ilo), xin(ihi)
          if ((ihi-ilo).ge.1)  then
             sum     = zero
             yout(i) = zero
             weight  = one
             do 50 j = ilo, ihi 
cc                this would provide a lorenztian weighting....
cc                weight = one /(((x(j)-x0)*(x(j)-x0)) + gam2)
                sum    = sum     +        weight
                yout(i)= yout(i) + yin(j)*weight
 50          continue 
             yout(i)   = yout(i) / max(tiny,sum)
          else 
c if we have no input data points in this region, 
c use quadratic interpolation of available data
             iqt = ilo
             call qintrp(xin,yin,nxin,x0,iqt,yout(i))
          endif
 100   continue 
       do 200 i = 1, nxout
          xout(i) = yout(i)
 200   continue 
       return
c  end subroutine boxcar_interp
       end

       subroutine res_penalty(x0, nx0, x1, nx1, x2, nx2)

       include 'maxpts.h'
       integer    nx0, nx1, nx2
       double precision  x0(*), x1(*), x2(*)
       double precision xlo, xhi

       xlo = x1(1)
       xhi = x0(1)
       
cc       print*, 'bound  xlo, xhi = [ ', xlo, ' : ', xhi, ' ]'
cc       print*, 'bound  x = ', x2(1)

       if ((x2(1).ge.xlo) .and. (x2(1).le.xhi)) then
          x0(1) = 0
       elseif (x2(1).gt.xlo) then
          x0(1) = abs(x2(1) - xhi)
       elseif (x2(1).lt.xlo) then
          x0(1) = abs(x2(1) - xlo)
       endif

cc     print*, 'bound  penalty = ', x0(1)
       return
       end

     
