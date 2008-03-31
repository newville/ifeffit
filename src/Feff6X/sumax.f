c SUBROUTINE SUMAX (NPTS, RN, ANN, AA2, AASUM)
c This is a version of the subroutine sumax found on page 110 of
c Louck's book.  It performs eq 3.22, using simpson's rule and
c taking advantage of the logarithmic grid so that sum f(r)*dr becomes
c sum over f(r)*r*(0.05).  Linear interpolation is used at the end
c caps.  This version does not sum over 14 shells of identical
c atoms, instead it averages the contribution of one or more atoms
c of type 2 at the location of atom 1.  Louck's description (except
c for his integration algorithm) is very clear.
c
c input:  npts      number of points to consider
c         rn        distance from atom 1 to atom 2 in au
c         ann       number of type 2 atoms to add to atom 1, can
c                   be fractional
c         aa2(i)    potential or density at atom 2
c output: aasum(i)  spherically summed contribution added into this
c                   array so that sumax can be called repeatedly
c                   and the overlapped values summed into aasum
c
c Note that this routine requires that all position data be on a
c grid  rr(j) = exp (-8.8d0 + (j-1)*0.05d0), which is the grid
c used by Louck, and also used by ATOM if nuclear options not used.
c
c Coded by Steven Zabinsky, December 1989
c Modified for FEFF cluster code, August 1990, siz
c Bug fixed, May 1991, SIZ
c Another bug fixed, Mar 1992, SIZ
c
c T.L.Louck, "Augmented Plane Wave Method", W.A.Benjamin, Inc., 1967

      subroutine sumax (npts, rn, ann, aa2, aasum)
      implicit double precision (a-h, o-z)
      double precision delta, c88, half
      parameter (delta = 0.05d0, c88 = 8.8d0, half= 0.5d0)

      parameter (nptx=251)
      dimension aa2(nptx), aasum(nptx)
      dimension stor(nptx)
      external xx
c     jjchi     index beyond which aa2 is zero
c     jtop      index just below distance to neighbor
c               aasum is calculated only up to index jtop

c     Wigner-Seitz radius is set to 15 in ATOM.
      rws = 15
      jjchi = ii(rws)
      jtop  = ii(rn)

      topx = xx(jjchi)

      do 120  i = 1, jtop
         x = xx(i)
         xint = 0
         et = exp(x)
         blx = log(rn-et)
         if (blx .ge. topx)  goto 119
         jbl = 2+20*(blx+c88)
         if (jbl .lt. 1)  jbl=1
         if (jbl .ge. 2)  then
c           use linear interp to make end cap near center of neighbor
            xjbl = jbl
            xbl = delta * (xjbl-1.0) -c88
            g = xbl-blx
            xint = xint+ half*g*(aa2(jbl)*(2-20*g)*exp(2*xbl)
     1             +20*g*aa2(jbl-1)*exp(2*(xbl-delta)))
         endif
         tlx = log(rn+et)
         if (tlx .ge. topx)  then
            jtl = jjchi
            go to 90
         endif
         jtl = 1 + 20*(tlx+c88)
         if (jtl .lt. jbl)  then
c           handle peculiar special case at center of atom 1
            fzn = aa2(jtl)*exp(2*(xbl-delta))
            fz3 = aa2(jbl)*exp(2*xbl)
            fz2 = fzn+20*(fz3-fzn)*(tlx-xbl+delta)
            fz1 = fzn+20*(fz3-fzn)*(blx-xbl+delta)
            xint = half*(fz1+fz2)*(tlx-blx)
            go to 119
         endif
         xjtl = jtl
         xtl = delta*(xjtl-1)-c88
         c = tlx-xtl
         xint = xint+ half*c*(aa2(jtl)*(2-20*c)
     1         *exp(2*xtl)+aa2(jtl+1)*20*c
     2         *exp(2*(xtl+delta)))

   90    if (jtl .gt. jbl)  then
  100       xint = xint+half*(aa2(jbl)*exp(2*xbl)+aa2(jbl+1)
     1             *exp(2*(xbl+delta)))*delta
            jbl = jbl+1
            if (jbl .lt. jtl) then
               xbl = xbl+delta
               go to 100
            endif
         endif
  119    stor(i) = half*xint*ann/(rn*et)
  120 continue

      do 190  i = 1, jtop
         aasum(i) = aasum(i) + stor(i)
  190 continue

      return
      end
