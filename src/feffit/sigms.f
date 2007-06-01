       subroutine sigms (tk, theta, rs, nleg, rat, iz, sig2)
c
c  copyright 1993  university of washington
c                  john rehr, steve zabinsky, matt newville
c
c  the following routines calculate the debye-waller factor for a
c  path based on the temperature, debye temperature, average
c  norman radius, atoms in the path, and their positions.
c  these routines come courtesy of jj rehr and si zabinsky.
c  i changed them a bit.   matt n
c-------------------------------------------------------------------
c
c  from s i zabinsky.
c  inputs:
c       tk      temperature in degrees k
c       theta   debye temp in degrees k
c       rs      average wigner seitz or norman radius in bohr
c                     averaged over entire problem:
c                     (4pi/3)*rs**3 = sum( (4pi/3)rnrm**3 ) / n
c                     (sum is over all atoms in the problem)
c       nleg    nlegs in path
c       rat     positions of each atom in path (in bohr)
c       iz      atomic number of each atom in path
c output:
c       sig2    debye waller factor
c  notes:
c     all units of distance in this routine are angstroms
c     there are nleg atoms including the central atom.
c     index 0 and index nleg both refer to central atom.
       implicit none
       integer   i, j, nlegx, nleg
       double precision  tk, theta, zero, two, rs, dist
       double precision  rij, rimjm, rijm, rimj, riim, rjjm
       double precision  ridotj, cimj, cosijm, cij, cimjm
       double precision  sig2,sig2ij, cijm
       parameter (nlegx = 7, zero = 0., two = 2.)
       double precision rat(3,0:nlegx)
       integer iz(0:nlegx)
c
       sig2   = zero
       do 800 i  = 1, nleg
         do 800 j = i, nleg
c
c  calculate r_i-r_i-1 and r_j-r_j-1 and the rest of the
c  distances, and get the partial cosine term:
c       cosine(i,j) = r_i.r_j / ((r_i - r_i-1) * (r_j - r_j-1))
           rij    = dist ( rat(1,i)  , rat(1,j)   )
           rimjm  = dist ( rat(1,i-1), rat(1,j-1))
           rijm   = dist ( rat(1,i)  , rat(1,j-1))
           rimj   = dist ( rat(1,i-1), rat(1,j)   )
           riim   = dist ( rat(1,i)  , rat(1,i-1))
           rjjm   = dist ( rat(1,j)  , rat(1,j-1))
           ridotj = (rat(1,i)-rat(1,i-1)) * (rat(1,j)-rat(1,j-1))
     $            + (rat(2,i)-rat(2,i-1)) * (rat(2,j)-rat(2,j-1))
     $            + (rat(3,i)-rat(3,i-1)) * (rat(3,j)-rat(3,j-1))
           cosijm = ridotj / (riim * rjjm)
c
c  call corrfn to get the correlations between atom pairs
           call corrfn (rij  , theta, tk, iz(i)  , iz(j)  , rs, cij)
           call corrfn (rimjm, theta, tk, iz(i-1), iz(j-1), rs, cimjm)
           call corrfn (rijm , theta, tk, iz(i)  , iz(j-1), rs, cijm)
           call corrfn (rimj , theta, tk, iz(i-1), iz(j)  , rs, cimj)
c
c  combine outputs of corrfn to give the debye-waller factor for
c    this atom pair.   !!! note: don't double count (i.eq.j) terms !!!
           sig2ij = ( cij + cimjm - cijm - cimj ) * cosijm / two
           if (j.eq.i) sig2ij = sig2ij / two
           sig2 = sig2 + sig2ij
 800   continue
       return
c  end subroutine sigms
       end
       subroutine corrfn(rij, theta, tk, iz1, iz2, rs, cij)
c
c  copyright 1993  university of washington
c                  john rehr, steve zabinsky, matt newville
c
c  subroutine calculates correlation function
c  c(ri, rj) = <xi xj> in the debye approximation
c
c            = (1/n)sum_k exp(ik.(ri-rj)) (1/sqrt(mi*mj))*
c                              (hbar/2w_k)*coth(beta hbar w_k/2)
c
c            = (3kt/mu w_d**2) * sqrt(mu**2/mi*mj) * int
c  where :
c       x        k_d*r (distance parameter)  r distance in angstroms
c       theta    debye temp in degrees k
c       tk       temperature in degrees k
c       temper   theta / tk = hbar omegad/kt
c       k_d      debye wave number = (6*pi**2 n/v)
c       n/v      free electron number density = 1/(4pi/3rs**3)
c       rs       wigner seitz or norman radius in bohr
c       ami      atomic mass at sites i in amu
c       amj      atomic mass at sites j in amu
c       int      int_0^1 (temper/x) dw sin(wx)coth(w*temper/2)
c
c  solution by numerical integration
c
c  parameters pi, bohr, con
c  con=hbar**2/kb*amu)*10**20   in ang**2 units
c  hbar=1.0549x10**-34 amu=1.65979x10-27kg kb=1.3807x10-23
       implicit none
       double precision rij, theta, tk, rs, cij
       double precision pi,one,athird,bohr,con,x,temper
       double precision ami,amj,xkd,xinteg,eps,atwts

       integer  iz1,iz2,nx
       parameter (pi = 3.14159 26535 89793 23846 26433d0)
       parameter (one = 1.d0, athird = 0.333333333333333d0)
       parameter (bohr = 0.529 177 249d0)
       parameter (con = 48.559d0)
       common /xtemp/ x, temper
       external atwts
c
c  theta in degrees k, t temperature in degrees k
       ami    = atwts(iz1)
       amj    = atwts(iz2)
       temper = theta / tk
       xkd    = (9*pi/2.d0)**(athird) / (rs * bohr)
       x      = xkd * rij
c  call numerical integration
       call bingrt (xinteg, eps, nx)
       cij  = (3.d0/2.d0) * xinteg * con / (theta* sqrt(ami*amj))
       return
c  end subroutine corrfn
       end
       double precision function debfun(w)
c
c  copyright 1993  university of washington
c                  john rehr, steve zabinsky, matt newville
c
c  debfun = (sin(w*x)/x) * coth(w*temper/2)
       implicit none
       double precision wmin, argmax,x ,temper, w, emwt
       double precision  argu
       parameter (wmin = 1.d-20, argmax = 50.d0)
       common /xtemp/ x, temper

c  allow t = 0 without bombing
       debfun = 2 / temper
       if (w.gt.wmin) then
          debfun  = w
          if (x.gt.0) debfun = sin(w*x) / x
          emwt = 0
          argu = w*temper
          if (argu.lt.argmax) emwt = exp(-argu)
          debfun = debfun * (1 + emwt) / (1 - emwt)
       end if
       return
c  end function debfun
       end
       subroutine bingrt (b, eps, n)
c
c  copyright 1993  university of washington
c                  john rehr, steve zabinsky, matt newville
c
c  subroutine calculates integrals between [0,1]  b = int_0^1 f(z) dz
c  by trapezoidal rule and binary refinement  (romberg integration)
c  coded by j rehr (10 feb 92)   see, e.g., numerical recipes
c  for discussion and a much fancier version
c-----------------------------------------------
c     del=dz  itn=2**n tol=1.e-5
c     starting values
c      implicit double precision (a-h,o-z)
c     error is approximately 2**(-2n) ~ 10**(-.6n)
c     so nmax=10 implies an error of 1.e-6
c
       implicit none
       double precision debfun, zero,one,two,three,four,tol
       double precision b, eps, zi,del, sum, bn, bo,bnp1
       integer n, itn,i, nmax
       parameter(nmax = 10, tol = 1.d-5)
       parameter(zero=0.d0, one=1.d0, two=2.d0)
       parameter(three=3.d0, four=4.d0)
       external debfun
c
       n   = 0
       itn = 1
       del = one
       bn  = (debfun(zero) + debfun(one)) / two
       bo  = bn
 10    continue
c  nth iteration
c  b_n+1=(b_n)/2+deln*sum_0^2**n f([2n-1]deln)
         n   = n + 1
         if (n.gt.nmax) go to 40
         del = del / two
         sum = zero
         do 20 i= 1, itn
            zi  = (two * i - 1) * del
            sum = sum + debfun(zi)
 20      continue
c     bnp1=b_n+1 is current value of integral
c     cancel leading error terms b=[4b-bn]/3
c     note: this is the first term in the neville table - remaining
c           errors were found too small to justify the added code
         bnp1= ( bn / two ) + del * sum
         b   = (four * bnp1 - bn) / three
         eps = dabs( (b - bo) / b)
         if (eps.lt.tol) goto 60
         bn  = bnp1
         bo  = b
         itn = itn * 2
       goto 10
  40   continue
       return
 60    continue
       return
c end subroutine bingrt
       end
       double precision function atwts (iz)
c
c  returns atomic weight from atom number (iz)
c
       double precision atmass(103)
       data atmass /
     1   1.0079, 4.0026, 6.941,  9.0122, 10.81,   12.01,
     2   14.007, 15.999, 18.998, 20.18,  22.9898, 24.305,
     3   26.982, 28.086, 30.974, 32.064, 35.453,  39.948,
     4   39.09,  40.08,  44.956, 47.90,  50.942,  52.00,
     5   54.938, 55.85,  58.93,  58.71,  63.55,   65.38,
     6   69.72,  72.59,  74.922, 78.96,  79.91,   83.80,
     7   85.47,  87.62,  88.91,  91.22,  92.91,   95.94,
     8   98.91,  101.07, 102.90, 106.40, 107.87,  112.40,
     9   114.82, 118.69, 121.75, 127.60, 126.90,  131.30,
     x   132.91, 137.34, 138.91, 140.12, 140.91,  144.24,
     1   145,    150.35, 151.96, 157.25, 158.92,  162.50,
     2   164.93, 167.26, 168.93, 173.04, 174.97,  178.49,
     3   180.95, 183.85, 186.2,  190.20, 192.22,  195.09,
     4   196.97, 200.59, 204.37, 207.19, 208.98,  210,
     5   210,    222,    223,    226,    227,     232.04,
     6   231,    238.03, 237.05, 244,    243,     247,
     7   247,    251,    254,    257,    256,     254,
     8   257/
       atwts = atmass(iz)
       return
c  end function atwts
       end
       double precision function dist (r0, r1)
c      find distance between cartesian points r0 and r1
       double precision r0(3), r1(3)
       dist = 0
       do 10  i = 1, 3
          dist = dist + (r0(i) - r1(i))**2
 10    continue
       dist = sqrt(dist)
       return
c  end function dist
       end
