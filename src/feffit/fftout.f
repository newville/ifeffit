       subroutine fftout(mpts, xdat, step, xlo, xhi, nout, npts, xout)
c convert complex data xdat to a real array, using only
c      that part of the complex array between [xlow, xhi].
       implicit none
       integer  mpts, npts, nout, nmin, npairs, i
       complex*16  xdat(mpts)
       double precision xout(npts), step, dxi, xlo, xhi, small, tiny
       parameter (tiny = 1.d-4, small = 1.d-2)
c      
       dxi    = 1 / max(tiny, step)
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
