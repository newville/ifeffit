      subroutine ovrlp (iph, iphat, xat, yat, zat, iatph,
     1      iz, nat, rho, vcoul, edens, vclap, rnrm)

c     Overlaps coulomb potentials and electron densities for current
c     unique potential
      implicit double precision (a-h, o-z)

      include 'const.h'
      include 'dim.h'

      integer iphat(natx)
      integer iatph(0:nphx)

      integer iz(0:nfrx)
      integer iph, nat, ifr, iat, i, infr, inat, iovr
      double precision rnn, ann, rlapx
      double precision xat(natx), yat(natx), zat(natx)


      double precision rho(251,0:nfrx)
      double precision vcoul(251,0:nfrx)
      double precision edens(nrptx,0:nphx)
      double precision vclap(nrptx,0:nphx)
      double precision rnrm(0:nphx)

c     find out which free atom we're dealing with
cc      ifr = ifrph(iph)
      ifr = iph

c     start with free atom values for current atom
      do 100  i = 1, 250
         vclap(i,iph) = vcoul(i,ifr)
         edens(i,iph) = rho  (i,ifr)
  100 continue

c overlap from geometry with model atom iat
      iat = iatph(iph)
c     overlap with all atoms within r overlap max (rlapx)
c     12 au = 6.35 ang  This number pulled out of a hat...

      rlapx = 12

c     inat is Index of Neighboring ATom
      do 110  inat = 1, nat
c     don't overlap atom with itself
         if (inat .eq. iat)  goto 110
         
c     if neighbor is too far away, don't overlap it
         rnn = sqrt( (xat(inat)-xat(iat))**2 +
     $        (yat(inat)-yat(iat))**2 + (zat(inat)-zat(iat))**2)
         
         if (rnn .gt. rlapx)  goto 110
         
cc         infr = ifrph(iphat(inat))
         infr = iphat(inat)
         call sumax (250, rnn, one, vcoul(1,infr), vclap(1,iph))
         call sumax (250, rnn, one, rho  (1,infr), edens(1,iph))
 110  continue

c     set norman radius
      call frnrm (edens(1,iph), iz(ifr), rnrm(iph))

      return
      end
