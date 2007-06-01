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
       integer          i, j, nleg, nlegx
       double precision tk, theta, rs, sig2, zero, two
       double precision rij, rimjm, rimj, riim, rjjm, ridotj, corrfn
       double precision  dist, cij, cimjm, cijm, cimj, sig2ij, rijm
       parameter (nlegx = 7, zero = 0.d0, two = 2.d0)
       double precision  rat(3,0:nlegx)
       integer iz(0:nlegx)
       external dist, corrfn
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
c
c  call corrfn to get the correlations between atom pairs
           cij   = corrfn (rij  , theta, tk, iz(i)  , iz(j)  , rs)
           cimjm = corrfn (rimjm, theta, tk, iz(i-1), iz(j-1), rs)
           cijm  = corrfn (rijm , theta, tk, iz(i)  , iz(j-1), rs)
           cimj  = corrfn (rimj , theta, tk, iz(i-1), iz(j)  , rs)
c
c  combine outputs of corrfn to give the debye-waller factor for
c    this atom pair.   !!! note: don't double count (i.eq.j) terms !!!
           sig2ij = (cij + cimjm - cijm - cimj)*ridotj/(riim * rjjm)
           if (j.eq.i) sig2ij = sig2ij / two
           sig2 = sig2 + sig2ij
 800   continue
       sig2 = sig2/2
       return
c  end subroutine sigms
       end
       double precision function corrfn(rij, theta, tk,iz1,iz2,rs)
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
       integer  iz1, iz2
       double precision rs, theta, tk,  rij, rx, tx
       double precision conr, conh, rmass, debint, at_weight
       parameter (conh = 72.8385d0, conr = 4.5693346333d0)
       external at_weight, debint
c
c  theta in degrees k, t temperature in degrees k
       rx     = conr  * rij / rs
       tx     = theta / tk
       rmass  = theta * sqrt( at_weight(iz1) *  at_weight(iz2))
       corrfn = conh  * debint (rx,tx) / rmass
       return
c  end subroutine corrfn
       end
       double precision function debfun(w,rx,tx)
c
c  copyright 1993  university of washington
c                  john rehr, steve zabinsky, matt newville
c
c  debfun = (sin(w*rx)/rx) * coth(w*tx/2)
c
       implicit none
       double precision  wmin, argmax, w, emwt
       parameter (wmin = 1.d-20, argmax = 50.d0)
       double precision rx, tx

c  allow t = 0 without bombing
       debfun = 2 / tx
       if (w.gt.wmin) then
          debfun = w
          if (rx.gt.0) debfun = sin(w*rx) / rx
          emwt   = exp(-min(w*tx,argmax))
          debfun = debfun * (1 + emwt) / (1 - emwt)
       end if
       return
c  end function debfun
       end
       double precision function debint (rx,tx)
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
       integer  nmax, n, itn, i
       double precision tol, zero, one, two, three, debfun
       double precision rx, tx
       parameter(nmax = 12, tol = 1.d-9)
       parameter(zero=0.d0, one=1.d0, two=2.d0, three=3.d0)
       double precision del, bn, bo, zi, sum, bnp1
       external debfun
c
       itn = 1
       del  = one
       debint = zero
       bn   = (debfun(zero,rx,tx) + debfun(one,rx,tx)) /2
       bo   = bn
       do 40 n = 1, nmax
c  nth iteration
c  b_n+1=(b_n)/2+deln*sum_0^2**n f([2n-1]deln)
         del = del / two
         sum = zero
         do 20 i= 1, itn
            zi  = (two * i - 1) * del
            sum = sum + debfun(zi,rx,tx)
 20      continue
c     bnp1=b_n+1 is current value of integral
c     cancel leading error terms b=[4b-bn]/3
c     note: this is the first term in the neville table - remaining
c           errors were found too small to justify the added code
         bnp1   = del * sum + (bn / two)
         debint = (4 * bnp1 - bn)/three
         if (abs( (debint - bo) / debint).lt.tol) goto 45
         bn   = bnp1
         bo   = debint
         itn  = itn * 2
 40    continue
 45    continue 
       return
c end function debint
       end
       double precision function dist (r0, r1)
c  find distance between cartesian points r0 and r1
       double precision   r0(3), r1(3)
       dist = 0.d0
       do 10  i = 1, 3
          dist = dist + (r0(i) - r1(i))**2
 10    continue
       dist = sqrt(dist)
       return
c  end function dist
       end
       subroutine cordby(x, nx, y, ny, ierr)
c
c  copyright 1993  university of washington      matt newville
c  copyright 1999  university of chicago         matt newville
c
c  calculate xafs sigma^2 from correlated debye model ala jj rehr
c  important note:   the path information is looked up in the
c      common block /fefdat/, using the path held in scalar 'path_index'
c
c     x  = theta on input, sigma2 on output
c     y =  temperature
c-
       include 'consts.h'
       include 'arrays.h'
       include 'feffit.h'
       include 'fefdat.h'
       include 'pthpar.h'
       double precision x(*), y(*),  getsca, out(maxpts)
       double precision tk, theta, sig2, rat(3,0:maxleg)
       double precision small, big
       parameter(small =1.d-5, big =1.d10)

       integer  ierr, iz(0:maxleg), i, j, ipth, jfeff
       integer  u2ipth, inpath, nx1, ny1, nptstk, ipt, ix, iy
       external getsca, u2ipth, nptstk
c      
       ierr = 0
       nx1  = nx
       ny1  = ny
       nx   = nptstk (nx1, ny1)

       ipth = max(1, int ( getsca('path_index',0)))
cc       print*, '  cordby: ', nx1, ny1, nx , ipth
       jfeff  = -1
       if (ipth.ge.1) then
          inpath = u2ipth(max(1, ipth))
          jfeff  = jpthff(inpath)
cc          print*, ' cordby: ', ipth, inpath, jfeff
          do 50 i = 0, nlgpth(jfeff)
             iz(i)   = izpth(i,jfeff)
             do 40 j = 1,3
                rat(j,i)  = ratpth(j,i,jfeff)
 40          continue
 50       continue
       end if
cc       print*, '  cordby 100   nx = ', nx
       do 100 ipt = 1, nx 
          ix     = min(nx1, ipt)
          iy     = min(ny1, ipt)
          theta  = max(small, min(big, x(ix)))
          tk     = max(small, min(big, y(iy)))
          sig2  = zero
          if (jfeff.ge.1) then
             call sigms(tk,theta,rwgpth(jfeff),
     $            nlgpth(jfeff),rat,iz,sig2)
          endif
          out(ipt) = sig2 
cc          print*, ' debye: ipt ' , ipt, tk, theta, sig2
 100   continue 
       do 120 ipt = 1, nx
          x(ipt) = out(ipt)
 120   continue 
       return
c  end subroutine cordby
       end
       subroutine eins(x, nx, y, ny, ierr)
c
c calculate debye waller in einstein model 
c  inputs
c     x  = theta on input, sigma2 on output
c     y  = temp
c
c  calculate sigma^2 in the eisntein model inputs:
c  returned:
c    x      sigma squared [AA^2] 
c                = factor / (theta*rmass*tanh(theta/2t))
c    with factor = hbarc*hbarc/(two * boltz * amu2ev)
c
c  copyright (c) 1998  matt newville
       include 'consts.h'
       include 'arrays.h'
       include 'fefdat.h'
       include 'pthpar.h'
       integer   nx, ny, ierr, i, ipth, nx1, ny1, nptstk, ipt
       integer  u2ipth, inpath, jfeff
       double precision  x(*), y(*), tmp, getsca, theta, tk
       double precision  out(maxpts), a
       double precision small, big, factor, rminv, at_weight
       parameter(factor = 24.25423371d0, small =1.d-5, big =1.d10)
       external  getsca, at_weight, nptstk, u2ipth
       ierr  = -1

       nx1  = nx
       ny1  = ny
       nx   = nptstk (nx1, ny1)
       ipth = max(1, int ( getsca('path_index',0)))
c  construct reduced mass (in amu) using function at_weight
       inpath = u2ipth(max(1, ipth))
       rminv  = zero
       jfeff  = jpthff(inpath)
       do 50 i = 1, nlgpth(jfeff)
          a     = at_weight( izpth(i, jfeff))
          rminv = rminv +  one /max(one, a)
  50   continue
       rminv  = factor*max(small, min(big, rminv))
c
c       print*, ' eins: ', rminv, nx, ipth, x(1), y(1), y(2)
c       print*,  inpath, jfeff, nlgpth(jfeff), 1./rminv
       do 100 ipt = 1, nx
          ix     = min(nx1, ipt)
          iy     = min(ny1, ipt)
          theta  = max(small, min(big, x(ix)))
          tk     = max(small, min(big, y(iy)))
          out(ipt) = zero
          ierr   = 0
c evaluate sigma2 in einstein model, and overwrite x
          if (ipth.ne.0) then
             out(ipt)= rminv/(tanh(theta/(2*tk))*theta)
          endif
 100   continue 
       do 120 ipt = 1, nx
          x(ipt) = out(ipt)
 120   continue 
       return
c end subroutine eins
       end

