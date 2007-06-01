       subroutine dkfcn(m,n,x,f,iflag)
c fitting function to match data mu to CL f''
c used by lmdif1, called in routine dkfit
       include "dkcom.f"
       integer m,n,iflag, i, ipos
       double precision x(n),f(m), e, fx
c
       ipos = 1
       if (f2tof1) then 
          do 100 i = 1, npts
             e    = energy(i) + x(1)
             call lintrp(energy,expdat,npts,e,ipos,fx)
             f(i) = -f2cl(i) + x(2) + x(3) * fx
     $            + e * (x(4) + x(5)  * e )
 100      continue 
       else
          do 200 i = 1, npts
             e    = energy(i) + x(1)
             call lintrp(energy,expdat,npts,e,ipos,fx)
             f(i) = -f1cl(i) + x(2) + x(3) * fx
     $            + e * (x(4) + x(5)  * e )
 200      continue 
       endif
ccc      print *, 'dkfcn ' , f2tof1, npts, x
       return
       end
