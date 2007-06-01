       subroutine fiterr(fcn,nfit,nvar,mfit,mvar,fbest,ftemp,fjac,
     $      alpha,jprint,istep,x,delta,correl,ierror,iflag)
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
c     error analysis for a fit using the minpack routines
c
c     given a subroutine, *fcn*, to generate a fitting function
c     with *nfit* evaluations from a set of *nvar* variables,
c     with best-fit values *x* and residuals *fbest* determined,
c     this will return the uncertainties in *x* to *delta*, and
c     the correlations between the variables in *correl*.
c
c  arguments:
c     fcn     name of subroutine to generate fitting function,    [in]
c             with call statement as for minpack routines :
c                   call fcn(nfit,nvar,x,f,ier)
c     nfit    number of function evaluations for call to fcn      [in]
c     nvar    number of variables                                 [in]
c     mfit    dimension of arrays for function evaluations        [in]
c     mvar    dimension of arrays for variables                   [in]
c     fbest   array of fit residual for best fit         (mfit)   [in]
c     ftemp   array of fit residuals for constructing    (mfit) [work]
c             jacobian. on output, this is equal to fbest.
c     fjac    array of finite difference jacobian   (mfit,mvar) [work]
c     alpha   curvature and covariance matrix       (mvar,mvar) [work]
c     jprint  integer print flag for debug messages               [in]
c     istep   maximum number of loops in error evaluation         [in]
c     x       array of best fit values for variables     (mvar)   [in]
c     delta   array of uncertainties for the variables   (mvar)  [out]
c     correl  array of two-variable correlations    (mvar,mvar)  [out]
c     ierror  integer flag that is non-zero if error bars        [out]
c             cannot be estimated because the curvature
c             matrix cannot be inverted, so that one or
c             more of the variables do not affect the fit.
c     iflag   integer array whose elements are 1 if the   (mvar) [out]
c             corresponding variable is suspected of
c             causing the failure of the inversion of the
c             curvature matrix. these may be null variables.
c
c  required external subprograms:
c     fcn,  gaussj 
c
c     the algorithm here is to construct and invert the nvar x nvar
c     curvature matrix  alpha, whose elements are found by summing
c     over the elements of the jacobian matrix, fjac:
c        fjac(i,j) = dfvect(i) / dx(j)   (i = 1, nfit; j = 1, nvar)
c     where fvect is the residual array for the fit and dx is a small
c     change in one variable away from the best-fit solution. then
c        alpha(j,k) = alpha(k,j)
c                   = sum_(i=1)^nfit (fjac(i,j) * fjac(i,k))
c
c     the inverse of alpha gives the curvature matrix, whose diagonal
c     elements are used as the uncertainties and whose off-diagonal
c     elements give the correlations between variables.
c--------------------------------------------------------------------
       implicit none
       integer  mfit,mvar,nfit,nvar,i,k,j,iloop,istep, istepx
       integer  iflag(mvar), ierror, jprint, ier
       double precision  fbest(mfit), ftemp(mfit), fjac(mfit,mvar)
       double precision  x(mvar), correl(mvar,mvar), alpha(mvar,mvar)
       double precision  delta(mvar), delx, sum, tempx
       double precision  eps, epsdef, tiny, zero
       character messg*64
       parameter (zero   = 0.d0, epsdef = 1.d-3, tiny= 1.d-12)
       external  fcn, gaussj
c
       if (jprint.ge.1)  call echo( '>>>> fiterr start')
       istepx= min(5,max(1, istep))
       ier   = 0
       ierror= 0
       iloop = 0

       do 3 j = 1, nvar
          delta(j) = zero
 3     continue 
 10    continue
       iloop = iloop  + 1
c
c     construct jacobian using the best possible guess for the
c     relative error in each variable to evaluate the derivatives.
c     if not available, use 1% of the value for the variable.
       do 50 j = 1, nvar
          tempx = x(j)
          if (iloop .eq. 1)   then
             delx = max( tiny, epsdef * abs(tempx) )
          else
             delx = max(tiny, abs(delta(j)))/2.d0
          endif
          x(j)  = tempx + delx
          if (jprint.ge.1) then
             write(messg,'(1x,a,3g14.7)') '  >> ',tempx,delta(j),delx
             call echo(messg)
          end if
          if (jprint.ge.4)  call echo( '>>>> call fcn' )
          call fcn(nfit, nvar, x, ftemp, ier)
          if (ier .lt. 0) then
             if (jprint.ge.1)  call echo( '>>>> fcn died')
             go to 65
          end if
          do 30 i = 1, nfit
             fjac(i,j) = ( fbest(i) - ftemp(i)) / delx
 30       continue
          x(j)  = tempx
 50    continue
 65    continue
c
c   re-evaluate best-fit to restore any common block stuff
       call fcn(nfit,nvar,x,ftemp,ier)
c
c     collect the symmetric curvature matrix, store in alpha
       if (jprint.ge.2)  then
          call echo( '   curvature matrix:  j , k , alpha(j,k)')
       end if
       do 180 j = 1, nvar
          do 160 k = 1, j
             sum  = zero
             do 140 i = 1, nfit
                sum = sum  + fjac(i,j) * fjac(i,k)
 140         continue
             alpha(j,k) = sum
             if (k.ne.j) alpha(k,j) = sum
             if (jprint.ge.2)  then
                write(messg,'(8x,2i3,g14.7)')  j , k , alpha(j,k)
                call echo(messg)
             end if
 160      continue
 180   continue
c
c     in case alpha cannot be inverted, flag those variables with
c     small diagonal components of alpha - these are the likely
c     null variables that caused the matrix inversion to fail.
       do 250 i = 1, nvar
          iflag(i) = 0
          if (abs(alpha(i,i)).le. tiny)  iflag(i) = 1
 250   continue
c  invert curvature (alpha) to give covariance matrix.  gaussj does
c  gauss-jordan elimination in-place, and dies with garbage in alpha
c  if the matrix is singular.
       if (jprint.ge.1) call echo(' fiterr-> call gaussj')
       call  gaussj(alpha,nvar,mvar,ier)
       if (jprint.ge.1) call echo(' fiterr-> gaussj returned')
       if (ier.ne.0) then
          ierror = 1
          if (jprint.ge.1) then
             call warn(2,'   FITERR:  cannot invert curvature matrix!')
          end if
          return
       end if
c
c     alpha now contains the covariance matrix, and is easily
c     converted into delta, the uncertainty for each variable,
c     and correl, the two-variable correlation matrix.
       if (jprint.ge.1)  then
          call echo(' fiterr done with loop:  j , delta(j)' )
       end if
       do 360 i = 1, nvar
          delta(i) = max(tiny, sqrt( abs( alpha(i,i)) ))
          if (jprint.ge.1) then
             write (messg,'(1x,i3,g15.7)') i, delta(i)
             call echo(messg)
          end if
          do 330 j = 1, i
             correl(j,i) =  alpha(j,i) / (delta(i) * delta(j))
             correl(i,j) = correl(j,i)
 330      continue
 360   continue
c
c     try it a second time with better estimates for the values
c     of deltax for the derivatives to get the jacobian matrix.
       if ( iloop .lt. istepx )  go to 10
c
c     finished
       if (jprint.ge.1)  call echo( '>>>> fiterr done')
       return
c     end routine fiterr
       end
