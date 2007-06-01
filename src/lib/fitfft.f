       subroutine fitfft(chiq, mpts, mfft, wfftc, qgrid,
     $      qwin, qweigh, rwin, rweigh, ifft, xlow, xhigh,
     $      nout, chifit)
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
c    calculate a fft of a function to be minimized in either r or
c    backtransformed k-space to use as a fitting function, as in 
c    ifeffit.  calls routine xafsft which uses the routine cfftf.
c
c    ** cffti must be called prior to this routine **
c
c inputs:
c   chiq    array containing chi(q), on grid with spacing qgrid, 
c           and first point at chi(q = 0.).
c   mpts    dimension of chiq, qwin, and rwin
c   mfft    number of points to use for fft 
c   wfftc   work array for fft initialized by cffti, which must
c           be called prior to this routine.
c   qgrid   grid size for chiq.
c   qwin    q-space fft window array 
c   qweigh  q-weight in  k->r fft.
c   rwin    r-space fft window array
c   rweigh  r-weight in  r->q fft.
c   ifft    integer flag for number of fft's to do:
c             0    chifit is in original k-space 
c             1    chifit is in r-space 
c             2    chifit is in back-transformed k-space 
c   xlow    low-x range for output chifit (either r or k)
c   xhigh   high-x range for output chifit (either r or k)
c   nout    number of points in output : useful length of chifit
c outputs:
c   chifit  real array representation of the complex result from 
c           0, 1, or 2 fft of the input chi(k).
c           output between xlow and xhigh in real-imag pairs
c           (if ifft=0, all imag parts are 0.) 
c
c mxmpts is the largest expected value for mpts
c
        implicit none
        integer   mpts, mfft, ifft, nout, mxmpts, nfft, i, ipos, jft
        double precision  pi, zero, xlow, xhigh, xgrid
        parameter (mxmpts = 4096, zero=0.d0, pi = 3.141592653589793d0)
        double precision chiq(mpts), chifit(mpts),qwin(mpts),rwin(mpts)
        double precision qweigh, rweigh, qgrid, rgrid, q, pha
        double precision  wfftc(*)
        complex*16  cchiq(mxmpts), tmpft(mxmpts), coni
        parameter (coni=(0d0,1d0))

c  check that ifft is valid
       if ((ifft.lt.0).or.(ifft.ge.3)) then 
          call warn(3,'fitfft: ifft out of range.')
          return
       endif
cc       if (mxmpts.ne.mfft) then 
cc          call echo('fitfft warning: weird number of points')
cc          print*, mxmpts, mfft
cc       endif

c
c  nfft will be the actual length of the fft arrays. 
c  it is expected that nfft = mfft, but just in case...
       nfft   =  min(mxmpts, min(mfft, mpts) )
       rgrid  =  pi / (qgrid * nfft)
c
c  copy input data into complex data array.
       do 130 i = 1, nfft
          cchiq(i) = dcmplx(chiq(i), zero)
 130   continue
c
c  do ifft (= 0, 1, 2)  number of fourier transforms
c  ifft: 
c     0   just get k-weighted chi(k)
c     1   k->r
c     2   k->r then r->q

       jft = 1
       if (ifft.eq.0) jft = 0

       xgrid = qgrid
       if (ifft.eq.1) xgrid = rgrid

       call xafsft(nfft,cchiq,qwin,qgrid,qweigh,wfftc, jft, tmpft)

       if (ifft.eq.2) then
          call xafsft(nfft,tmpft,rwin,rgrid,rweigh,wfftc,-1, cchiq)
          call fftout(mxmpts,cchiq,qgrid,xlow,xhigh,nout,mpts,chifit)
       else
          call fftout(mxmpts,tmpft,xgrid,xlow,xhigh,nout,mpts,chifit)
       endif
cc       if (ifft.eq.0) nout = nout/2
       return
c  end subroutine fitfft
       end

       subroutine fftout(mpts, xdat, dx, xlo, xhi, nout, npts, xout)
c convert complex data xdat to a real array, using only
c that part of the complex array between [xlow, xhi].
       integer  mpts, npts, nout, nmin, npairs, i
       complex*16  xdat(mpts)
       double precision xout(npts), dx, dxi, xlo, xhi, small, tiny
       parameter (tiny = 1.d-9, small = 1.d-2)
c
       dxi    = 1 / max(tiny, dx)
       nmin   = max(0, int(xlo * dxi + small ))
       npairs = max(1, int(xhi * dxi + small )) - nmin + 1
       nout   = min(npts, 2 * npairs)
       do 50 i= 1, npairs
          xout(2*i-1) = dble (xdat( nmin + i ))
          xout(2*i  ) = dimag(xdat( nmin + i ))
 50    continue
       return
c end subroutine fftout
       end
