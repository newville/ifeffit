c     make e and r mesh for phase
c     input:  nr, dx, x0, nemax, iprint,
c             ixanes, edge, xmu, vint, vr0, imt, edens, nph
c             edge, xmu... used only with ixanes = 1
c
c     output: ri(nr), ne, em(ne), ik0 [grid point with k=0]
c
c     set nemax = nex (from dim.h) for max number of points

      subroutine phmesh (nr,  nemax, iprint,
     1     edge, vint, vr0, imt, edens, nph,
     2     ri, ne, em, ik0)
      implicit double precision (a-h, o-z)
      include 'const.h'
      include 'dim.h'
      dimension ri(nr), em(nex)
      double precision vint, vr0
c     edens       overlapped density*4*pi
c     imt         r mesh index just inside rmt
c     see arrays.h
      dimension edens(nr,0:nphx)
      dimension imt(0:nphx)

cc      print*, ' phmesh ', nr, nphx, nph, nemax, iprint, edge, vint, vr0

c     r mesh
      do 100  i = 1, nr
         ri(i) = rr(i)
  100 continue

      n = 0

c     energy mesh
c      n pts (-2 le k lt 0,  delk=0.2 ang(-1) ) (only if xanes)
c     30 pts (0 le k le 5.8, delk=0.2 ang(-1) )
c      9 pts (6 le k le 10., delk=0.5 ang(-1) )
c     10 pts (11 le k le 20.0, delk=1.0 ang(-1) )
      ne = 0

cc      print*, ' phmesh 1 ', nph, nex
      delk = bohr/5
      do 112 i=1,30
         tempk=(i-1)*delk
         ne = ne+1
         em(ne)=tempk**2+edge
         if (i.eq.1)  ik0 = ne
  112 continue
      delk = bohr/2

      do 113 i=1,9
         tempk=6.*bohr + (i-1)*delk
         ne = ne+1
         em(ne)=tempk**2+edge
  113 continue
      delk=bohr

      do 114 i=1,10
         tempk=11.*bohr + (i-1)*delk
         ne = ne+1
         em(ne)=tempk**2+edge
  114 continue

c$$$      print*, 'phmesh: ne, nex, nemax before setting ne ',
c$$$     $     ne, nex, nemax
c$$$      ne = min (ne, nemax)
c$$$      print*, 'phmesh: ne, nex, nemax after  setting ne ',
c$$$     1                 ne, nex, nemax

      if (iprint .ge. 3)  then
         open (unit=44, file='emesh.dat')
         write(44,*) 'edge, bohr, edge*ryd ', edge, bohr, edge*ryd
         write(44,*) ' ik0 ',  ik0
         write(44,*) vint,  n, ' vint, n'
         write(44,*) 'ie, em(ie), xk(ie)'
         do 230  ie = 1, ne
            write(44,220)  ie, em(ie), getxk(em(ie)-edge)/bohr
  220       format (i5, 2f20.5)
  230    continue
         close (unit=44)
      endif

      return
      end
