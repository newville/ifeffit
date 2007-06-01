       double precision function cordby(temp, thetad, ierr)
c
c  copyright 1993  university of washington      matt newville
c
c  returns xafs sigma^2 from correlated debye model a la jj rehr
c  important note:   the path information is passed in the
c      common block /fefdat/, and the value ixpath  specifies
c      which path sigma^2 is to be calculated for.
c  temp    temperature       [k]      (in)
c  theta   debye temperature [k]      (in)
c  cordby  sigma^2           [aa^2]   (out)
c-
       include 'const.h'
       include 'fefdat.h'
       double precision temp, thetad, tk, theta, sig2
       double precision rat(3,0:maxleg)
       integer  ierr, iz(0:maxleg), i, j, ipth
c
       ierr   = 0
       cordby = zero
       ipth   = min( mfffil, max(1, ixpath))
c
       tk     = temp
       theta  = thetad
       if (tk   .le.one) tk    = one
       if (theta.le.one) theta = one
       do 50 i = 0, nlgpth(ipth)
          iz(i)   = izpth(i,ipth)
          do 40 j = 1,3
             rat(j,i)  = ratpth(j,i,ipth)
 40       continue
 50    continue
       call sigms (tk, theta, rwgpth(ipth), nlgpth(ipth),rat,iz,sig2)
       cordby = sig2
       return
c  end function cordby
       end
       double precision function einsdw(temp, theta, ierr)
c
c  copyright 1994  university of washington      matt newville
c
c  return sigma^2 factor from einstein model.
c  important note:   the path information is passed in the
c      common block /fefdat/, and the value ixpath  specifies
c      which path sigma^2 is to be calculated for.
c   temp    temperature           [k]      (in)
c   thetae  einstein temperature  [k]      (in)
c   einsdw  sigma^2               [aa^2]   (out)
c-
       double precision   small
       parameter (small = 1.d-4)
       include 'const.h'
       include 'fefdat.h'
       double precision temp, theta, rmass, rtemp, atwts, einval
       integer  ierr, ipth, i, natoms
       external atwts, einval
       ierr   = 0
       einsdw = zero
       ipth   = min( mfffil, max(1, ixpath))
c  construct reduced mass (in amu) using function atwts
       natoms = nlgpth(ipth)
       rtemp  = zero
       rmass  = zero
       do 50 i = 1, natoms
          rmass  = max(one, atwts( izpth(i, ipth)) )
          rtemp  = rtemp +  one / rmass
  50   continue
       rmass  = max(small, rtemp)
       rmass  = one / rmass
c  call einval to get sigma squared
       einsdw = einval(temp, theta, rmass)
       return
c  end function einsdw
       end
       double precision function einval(t,theta,rmass)
c
c  compute sigma^2 from the eisntein model
c    t       temperature          [K]    (in)
c    theta   einstein temperature [K]    (in)
c    rmass   reduced mass         [amu]  (in)
c    einval  sigma squared        [aa^2] (out)
       double precision  small, big, two, factor
       double precision  t, theta, rmass, x, t1, th1, rm1
       parameter (two    = 2.d0, small = 1.d-3, big = 1.d8   )
       parameter (factor = 24.25423371d0)
cc       parameter (hbarc  = 1973.270533d0, boltz = 8.617385d-5)
cc       parameter (amu2ev = 9.3149432d8          )
cc       parameter (factor = hbarc*hbarc/(two * boltz * amu2ev))
       th1  = max(small, theta)
       rm1  = max(small, rmass)
       t1   = max(small, t    )
       x    = max(small, min(big, th1 / (two * t1)))
       einval = factor / ( rm1 * th1 * tanh(x))
       return
c  end function einval
       end
