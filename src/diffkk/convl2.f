       subroutine convl2(npts, x, y, gamma, xout, yout)
c  broaden the two arrays x and y with a lorentzian 
c  arrays are assumed to be on the same even grid
c
c  note: gamma is the lorentzian width in units of grid points!!
       implicit none
       integer npts, i, j
       double precision x(npts), y(npts), xout(npts), yout(npts)
       double precision gamma, sum, factr, zero, one, small
       double precision  tmpx, tmpy, gami2
       parameter  (zero = 0.d0, one = 1.d0, small = 1.d-20)

      gamma = max(small, gamma)
      gami2 = one /(gamma*gamma)
       do 100 i = 1, npts
          sum  = zero
          tmpx = zero
          tmpy = zero
          do 50 j = 1, npts
             factr = 1 / ( 1 + gami2 * (j-i) * (j-i) )
             sum   = sum  + factr
             tmpx  = tmpx + x(j) *  factr
             tmpy  = tmpy + y(j) *  factr
 50       continue 
          yout(i) = tmpy / max (small, sum)
          xout(i) = tmpx / max (small, sum)
 100   continue 
       return
       end
