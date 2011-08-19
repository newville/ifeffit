      subroutine snlm (ltot, mtot, xnlm)
      implicit none
c     Set nlm, legendre normalization factors, xnlm in common /nlm/
c     Calculates legendre norm factors
c     xnlm= sqrt ((2l+1)(l-m)!/(l+m)!)

cc      include 'dim.h'
      integer ltot, mtot, il, im, mmaxp1, l, m, i
      double precision xnlm(ltot+1, mtot+1)

c     flg(i) = i! * afac**i, set in factst
      double precision flg(0:210), afac, cnlm

      call factst (afac, flg)

c     initialize xnlm explicitly
      do 20  il = 1, ltot+1
         mmaxp1 = min(mtot+1, il)
         do 5  im = mmaxp1, mtot+1
            xnlm(il, im) = 0.d0
 5       continue
         do 10  im = 1, mmaxp1
            l = il-1
            m = im-1
            cnlm = (2*l+1) * flg(l-m) / flg(l+m)
            xnlm(il,im) = sqrt(cnlm) * afac**m
 10      continue
 20   continue
      return
      end
      subroutine factst (afac, flg)
      implicit none

c     FACTorial SeT, flg(i) = i! * afac**i
      double precision afac, flg(0:210)
      integer i
c     afac = 1/64 works with double precision on a VAX
      afac = 1/64.d0

      flg(0) = 1
      flg(1) = afac
      do 10  i = 2, 210
         flg(i) = flg(i-1) * i * afac
 10   continue
      return
      end
