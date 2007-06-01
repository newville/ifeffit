c
c//////////////////////////////////////////////////////////////////////
c Copyright (c) 1997--2001 Matthew Newville, The University of Chicago
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
       subroutine w_fftf(x,nx,ier)
c wrapper for decod calling fftf
       include 'consts.h'
       include 'fft.h'
       double precision x(*)
       complex*16       xt(maxfft)
       integer          nx,ier, mfft
       mfft = maxfft
       do 10 i = 1, nx
          xt(i)  =  cmplx(x(i), zero)
 10    continue 
       do 12 i = nx+1, maxfft
          xt(i)  =  cmplx(zero,zero)
 12    continue 
       call cfftf(mfft,xt,wfftc)
       do 20 i = 1, nx
          x(i) = dble(xt(i))
 20    continue 
cc       print*, ' : : ', x(1), x(3), x(5)
       
       ier = 0
       return
       end
c
       subroutine w_fftr(x,nx,ier)
c wrapper for decod calling fftr
       include 'consts.h'
       include 'fft.h'
       double precision x(*)
       integer          nx,ier
       call cfftb(nx,x,wfftc)
       ier = 0
       return
       end
c
       subroutine w_kkf(x,nx,y,ny,ier)
c wrapper for decod calling kkmclf
       include 'consts.h'
       double precision x(*),y(*), out(maxpts)
       integer          nx,ny,ier
       call kkmclf(ny,y,x,out)
       do 10 i = 1, ny
          x(i) = out(i)
 10    continue 
       ier = 0
       return
       end
c
       subroutine w_kkr(x,nx,y,ny,ier)
c wrapper for decod calling kkmclr
       include 'consts.h'
       double precision x(*),y(*), out(maxpts)
       integer          nx,ny,ier
       call kkmclr(ny,y,x,out)
       do 10 i = 1, ny
          x(i) = out(i)
 10    continue 
       ier = 0
       return
       end
c
       subroutine kkmclr(npts, e, finp, fout)
c  reverse (f''->f') kk transform, using maclaurin series algorithm
c  arguments: 
c    npts   size of arrays to consider 
c    e      energy array *must be on an even grid*  [npts]  (in)
c    finp   f''    array                            [npts]  (in)
c    fout   f'     array                            [npts]  (out)
c m newville  jan 1997
       implicit none
       double precision  e(*), finp(*), fout(*)
       double precision  factor, ei2, de2, fopi, zero, tiny
       parameter(fopi = 1.273239544735163d0, zero = 0.d0, tiny=1.d-20)
       integer   npts, i, j, k, ioff, nptsk
       
       if (npts.ge.2) then 
          factor = - fopi * (e(npts) - e(1)) / (npts - 1)
          nptsk  = npts / 2
          do 100 i=1, npts
             fout(i) = zero
             ei2    = e(i) * e(i)
             ioff   = mod(i,2) - 1
             do 50 k = 1, nptsk
                j    = k + k + ioff
                de2  = e(j)*e(j) - ei2
                if (abs(de2).le.tiny) de2 = tiny
                fout(i) = fout(i) + e(j) * finp(j) / de2
 50          continue 
             fout(i) = factor * fout(i) 
 100      continue 
       end if
       return
c end subroutine kkmclr
       end
       subroutine kkmclf(npts, e, finp, fout)
c  forward (f'->f'') kk transform, using  mclaurin series algorithm
c  arguments: 
c    npts   size of arrays to consider 
c    e      energy array *must be on an even grid* [npts]  (in)
c    finp   f'     array                           [npts]  (in)
c    fout   f''    array                           [npts]  (out)
c  notes  fopi = 4/pi
       implicit none
       double precision  e(*), finp(*), fout(*)
       double precision  factor, ei2, de2, fopi, zero, tiny
       parameter(fopi = 1.273239544735163d0, zero = 0.d0, tiny=1.d-20)
       integer   npts, i, j, k, ioff, nptsk
       
       if (npts.ge.2) then 
          factor = fopi * (e(npts) - e(1)) / (npts - 1)
          nptsk  = npts / 2
          do 100 i=1, npts
             fout(i) = zero
             ei2     = e(i) * e(i)
             ioff    = mod(i,2) - 1
             do 50 k = 1, nptsk
                j    = k + k + ioff
                de2  = e(j)*e(j) - ei2
                if (abs(de2).le.tiny) de2 = tiny
                fout(i) = fout(i) + finp(j) / de2
 50          continue 
             fout(i) = factor * fout(i) * e(i)
 100      continue 
       end if
       return
c end subroutine kkmclf
       end
