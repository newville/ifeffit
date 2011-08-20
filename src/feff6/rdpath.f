      subroutine read_pathgeom(in, ipath, nleg, npot, deg,
     $     rat, ipot, potlbl, done)
c
c read path geometry paragraph from open paths.dat file (iunit=in)
c no calculation here, just read/parse file
c
      implicit none
      include 'dim.h'

      logical done
      integer ipath, nleg, npot, ipot(0:legtot)
      double precision rat(3,0:legtot+1), deg
      character*6  potlbl(0:npotx)
      character*128 messag
      integer ileg, j, in

      done = .false.
      read(in, *, end=100)  ipath, nleg, deg
      if (nleg .gt. legtot)  then
         write(messag,'(1x,a,2i5)')
     $        ' RDPATH: nleg > legtot: nleg, legtot ', nleg, legtot
         call echo(messag)
         goto 100
      endif

c     skip label (x y z ipot rleg beta eta)
      read(in,*)
      do 20  ileg = 1, nleg
         read(in, *, end=105)  (rat(j,ileg),j=1,3), ipot(ileg),
     $        potlbl(ipot(ileg))
         if (ipot(ileg) .gt. npot)  then
            write(messag,'(1x,a,3i5)')
     $           ' RDPATH: ipot(ileg) too big: ipot, ileg, npot ',
     $           ipot(ileg), ileg, npot
            call echo(messag)
            goto 100
         endif
 20   continue
      return
c at end of data: set done to true.
 100  continue
      done = .true.
      return
c on error:
  105 continue
      call fstop(' at RDPATH: unexpected end of file')

      end
c
      subroutine calc_pathgeom(nleg, rat, ipot, ri, beta, eta)
c
c do geometry calcs on path geometry, as read from paths.dat
c   input:  nleg, rat, ipot
c    output: rat (now in code units), ri, beta, eta
      implicit none
      include 'const.h'
      include 'dim.h'
      include 'pola.h'

      double precision rat(3,0:legtot+1)
      double precision ri(legtot), beta(legtot+1), eta(0:legtot+1)
      integer ipot(0:legtot)
      complex*16  alph, gamm
      double precision alpha(0:legtot), gamma(legtot)
      double precision x, y, z, sp, spp, sppp, st, stp, phip, phi
      double precision dumm, ctp, ct, cp, cpp, cppp, dist
      character messag*128
      integer ileg, nleg, j, i, ifix, im1, ip1, nangle, nsc
      external dist

c convert rat to code units
      do 20  ileg = 1, nleg
         do 10  j = 1, 3
            rat(j,ileg) = rat(j,ileg)/bohr
 10      continue
 20   continue
      nsc = nleg-1

c     We need the 'z' atom so we can use it below.  Put
c     it in rat(nleg+1).  No physical significance, just a handy
c     place to put it.
      if (pola) then
         rat(1,nleg+1) = rat(1,nleg)
         rat(2,nleg+1) = rat(2,nleg)
         rat(3,nleg+1) = rat(3,nleg) + 1.d0
      endif

c     add rat(0) and ipot(0) (makes writing output easier)
      do 22 j = 1, 3
         rat(j,0) = rat(j,nleg)
   22 continue
      ipot(0) = ipot(nleg)

      nangle = nleg
      if (pola) then
c        in polarization case we need one more rotation
         nangle = nleg + 1
      endif
      do 100  j = 1, nangle

c        for euler angles at point i, need th and ph (theta and phi)
c        from rat(i+1)-rat(i)  and  thp and php
c        (theta prime and phi prime) from rat(i)-rat(i-1)
c
c        Actually, we need cos(th), sin(th), cos(phi), sin(phi) and
c        also for angles prime.  Call these  ct,  st,  cp,  sp

c        i = (j)
c        ip1 = (j+1)
c        im1 = (j-1)
c        except for special cases...
         ifix = 0
         if (j .eq. nsc+1)  then
c           j+1 'z' atom, j central atom, j-1 last path atom
            i = 0
            ip1 = 1
            if (pola) then
               ip1 = nleg+1
            endif
            im1 = nsc

         elseif (j .eq. nsc+2)  then
c           j central atom, j+1 first path atom, j-1 'z' atom
            i = 0
            ip1 = 1
            im1 = nleg+1
            ifix = 1
         else
            i = j
            ip1 = j+1
            im1 = j-1
         endif

         x = rat(1,ip1) - rat(1,i)
         y = rat(2,ip1) - rat(2,i)
         z = rat(3,ip1) - rat(3,i)
         call trig (x, y, z, ctp, stp, cpp, spp)
         x = rat(1,i) - rat(1,im1)
         y = rat(2,i) - rat(2,im1)
         z = rat(3,i) - rat(3,im1)
         call trig (x, y, z, ct, st, cp, sp)

c   Handle special case, j=central atom, j+1 first
c   path atom, j-1 is 'z' atom.  Need minus sign
c   for location of 'z' atom to get signs right.
         if (ifix .eq. 1)  then
            x = 0
            y = 0
            z = 1.0
            call trig (x, y, z, ct, st, cp, sp)
            ifix = 0
         endif

c   cppp = cos (phi prime - phi)
c   sppp = sin (phi prime - phi)
         cppp = cp*cpp + sp*spp
         sppp = spp*cp - cpp*sp
         phi  = atan2(sp,cp)
         phip = atan2(spp,cpp)

c        alph = exp(i alpha)  in ref eqs 18
c        beta = cos (beta)
c        gamm = exp(i gamma)
         alph = -(st*ctp - ct*stp*cppp - coni*stp*sppp)
         beta(j) = ct*ctp + st*stp*cppp
c        watch out for roundoff errors
         if (beta(j) .lt. -1) beta(j) = -1
         if (beta(j) .gt.  1) beta(j) =  1
         gamm = -(st*ctp*cppp - ct*stp + coni*st*sppp)
         call arg(alph,phip-phi,alpha(j))
         beta(j) = acos(beta(j))
         call arg(gamm,phi-phi,gamma(j))
c       Convert from the rotation of FRAME used before to the rotation
c       of VECTORS used in ref.
         dumm = alpha(j)
         alpha(j) =  pi- gamma(j)
         gamma(j) =  pi- dumm

         if (j .le. nleg)  then
            ri(j) = dist (rat(1,i), rat(1,im1))
         endif
  100 continue

c     Make eta(i) = alpha(i-1) + gamma(i).
c     We'll need alph(nangle)=alph(0)
      alpha(0) = alpha(nangle)
      do 150  j = 1, nleg
         eta(j) = alpha(j-1) + gamma(j)
  150 continue
      if (pola) then
         eta(0) = gamma(nleg+1)
         eta(nleg+1) = alpha(nleg)
      endif

      return
      end

      subroutine trig (x, y, z, ct, st, cp, sp)
      implicit double precision (a-h, o-z)
c     returns cos(theta), sin(theta), cos(phi), sin(ph) for (x,y,z)
c     convention - if x=y=0 and z>0, phi=0, cp=1, sp=0
c                  if x=y=0 and z<0, phi=180, cp=-1,sp=0
c                - if x=y=z=0, theta=0, ct=1, st=0
      parameter (eps = 1.0d-6)
      r = sqrt (x**2 + y**2 + z**2)
      rxy = sqrt (x**2 + y**2)
      if (r .lt. eps)  then
         ct = 1
         st = 0
      else
         ct = z/r
         st = rxy/r
      endif
      if (rxy .lt. eps)  then
         cp = 1
         if (ct .lt. 0) cp = -1
         sp = 0
      else
         cp = x / rxy
         sp = y / rxy
      endif
      return
      end
c
      subroutine arg(c,fi,th)
      implicit double precision (a-h, o-z)
      complex*16  c
      parameter (eps = 1.0e-6)
      x = dble(c)
      y = dimag(c)
      if (abs(x) .lt. eps) x = 0
      if (abs(y) .lt. eps) y = 0
      if (abs(x) .lt. eps  .and.  abs(y) .lt. eps) then
         th = fi
      else
         th = atan2(y,x)
      endif
      return
      end
