       subroutine xafsft(mpts, chip, wa, xgrid, xwgh, wfftc,jfft,chiq)
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
c  xafs fourier transform. includes k-weighting, an arbitrary window
c  function, and mapping from FT conjugates (k,2R) to (k,R), with
c  rational normalization
c
c  fft routines cfftf/b (from fftpack) are used in subroutine xfft.
c
c  arrays wa and wfftc must be initialized before this routine:
c      wfftc  must be initialized by "cffti".
c      wa     is probably initialized by "window".
c  arguments
c    mpts     dimension of arrays chip and wa                  [in]
c    chip     complex array of input data, on uniform grid     [in]
c             chip(1) = chi(x=0.), zero-padding expected.
c    wa       real array of window function                    [in]
c    xgrid    grid spacing for chip                            [in]
c    xwgh     x-weight                                         [in]
c    wfftc    work array for fft                               [in]
c    jfft     integer controlling functionality                [in]
c               1   forward transform (k->r)
c               0   no transform (returns windowed data)
c              -1   reverse transform (r->k)
c    chiq     complex fourier transform of chip               [out]
c
       implicit none
       integer  i, mpts, jfft, ixwgh
       double precision  wfftc(*), wa(*), xwgh, dx, xgrid
       double precision  sqrtpi, eps7, eps4
       complex*16  chip(*), chiq(*), cnorm
       parameter(sqrtpi = 0.5641895835d0, eps7=1.d-7, eps4=1.d-4)
c                sqrtpi = 1 / sqrt(pi)
c complex normalization constant, for the transform from r to k in
c    xafs, the xgrid is assumed to be the grid in r *not* in 2r.
c    to normalize correctly, cnorm must be multiplied by 2.
c    note that if we're not doing fft, we don't want to normalize
       cnorm = xgrid * sqrtpi * (1d0,0d0)
       if (jfft.lt.0) cnorm = 2 * cnorm
       if (jfft.eq.0) cnorm = (1d0,0d0)
c make chiq as  k-weighted and windowed chip
c   if xwgh is really an integer, do only the integer exponentiation
       ixwgh = int(xwgh)
       chiq(1) = (0d0,0d0)
       do 50 i = 2, mpts
          chiq(i) = cnorm * chip(i) * wa(i)
     $         * ((i-1) * xgrid)**ixwgh
 50    continue
c   do fp exponentiation only if it will be noticeable
       dx = xwgh - ixwgh
       if (dx .gt. eps4) then
          do 60 i = 1, mpts
             chiq(i) = chiq(i) * ((i-1)*xgrid)**dx
 60       continue
       end if
cc       print*, 'xafsft ' , mpts, jfft, wfftc(1), wfftc(40)
c do fft on modified array, chiq (fft is done in place):
c    jfft > 0:  cfftf, k->r, forward fft
c    jfft < 0:  cfftb, r->k, reverse fft
c    jfft = 0:  no fft, chiq returned as is (ie, after weighting)
       if (jfft.gt.0) call cfftf(mpts,chiq,wfftc)
       if (jfft.lt.0) call cfftb(mpts,chiq,wfftc)
       return
c  end subroutine xafsft
       end






