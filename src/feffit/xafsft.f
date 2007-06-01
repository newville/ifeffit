       subroutine xafsft(mpts, chip, wa, xgrid, xwgh, wfftc,jfft,chiq)
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
c  copyright 1997   matthew newville
c--------------------------------------------------------------------
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
       cnorm = xgrid * sqrtpi * (1.d0,0.d0)
       if (jfft.lt.0) cnorm = 2 * cnorm
       if (jfft.eq.0) cnorm = (1.d0,0.d0)
c make chiq as  k-weighted and windowed chip
c   if xwgh is really an integer, do only the integer exponentiation
       ixwgh = int(xwgh)
       if (ixwgh.eq.0) then 
          do 40 i = 1, mpts
             chiq(i) = (0,0)
             if (wa(i) .gt. eps7) chiq(i) = cnorm * chip(i) * wa(i)
 40       continue
       else 
          do 50 i = 1, mpts
             chiq(i) = (0,0)
             if (wa(i) .gt. eps7) chiq(i) = cnorm * chip(i) * wa(i)
     $            * ((i-1) * xgrid)**ixwgh
 50       continue
       end if
c   do fp exponentiation only if it will be noticeable
       dx = xwgh - ixwgh
       if (dx .gt. eps4) then
          do 60 i = 1, mpts
             if (wa(i).gt.eps7) chiq(i) = chiq(i) * ((i-1)*xgrid)**dx
 60       continue
       end if
c do fft on modified array, chiq (fft is done in place):
c    jfft > 0:  cfftf, k->r, forward fft
c    jfft < 0:  cfftb, r->k, reverse fft
c    jfft = 0:  no fft, chiq returned as is (ie, after weighting)
       if (jfft.gt.0) call cfftf(mpts,chiq,wfftc)
       if (jfft.lt.0) call cfftb(mpts,chiq,wfftc)
       return
c  end subroutine xafsft
       end
