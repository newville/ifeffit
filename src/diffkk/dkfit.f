       subroutine  dkfit
c     part of diffkk
c  match the externally supplied mu(E) to f'' from CL, allowing
c  a band-limited differential KK transform to be done.  here, we
c  set up for, and call to lmdif1, for a non-linear least-squares 
c  fit using subroutine dkfcn.
       include "dkcom.f"
       integer   lenwrk, lenfvc, lminfo, nofx, i, iwork(mvarys)
       parameter(lenwrk = 2*mpts*(mvarys + 1)  + 20*mvarys )
       parameter(lenfvc = mpts)
       double precision  work(lenwrk), fvect(lenfvc), toler, etmp
       parameter (toler = 1.d-5)
       external  nofx, dkfcn

c make sure e0 is set
       if (e0.le.0) call findee(npts, energy, expdat, e0)
c find useful indices in the energy array
       nelo   = 1 
       nehi   = npts
       ne0    = min(npts, max(1, nofx( e0  , energy, npts)) )
       ne0ish = ne0 - 5
       ne0dif = 200
c initialize variables
       do 10 i = 1, mvarys
          xvarys(i) = 0.d0
 10    continue
c xvarys(3) should be ~1
       xvarys(3) = 1.0
       numvar = 4
       lminfo = 0
c  call lmdif1

       call lmdif1 (dkfcn, npts, numvar, xvarys, fvect,
     $      toler, lminfo, iwork, work, lenwrk)
c done!
       return
       end
