       subroutine Potentials(geomfile, potfile,
     $     ihole, iexch, viexch, vrexch, rsexch,istat)

c 
c wrapper for Feff's Potentials Calculation
c     
      implicit none
      include 'dim.h'

      character*(*)     geomfile, potfile
      double precision  viexch, vrexch, rsexch
      integer iexch, ihole, istat

      character*1024  line, tmpstr
      integer istrln, ilen, jlen, iflen, iret, i, ierr, ios
      integer mwords, nwords, ititle, jstat, ipot0, itmp,ifr, nph
      integer mrpts
      logical debug
      parameter (mwords = 16)
      parameter (mrpts = 251)

      double precision  xnatph(0:nphx), vcoul(mrpts,0:nphx)
      double precision  dgc0(mrpts), dpc0(mrpts)
      double precision  rho(mrpts,0:nphx)
      double precision  xf, xmu, x0, rs, rhoint, rnrmav, vint, edge
      double precision dx,  em
      integer  imt(0:nphx)	!r mesh index just inside rmt
      integer  inrm(0:nphx)	!r mesh index just inside rnorman
      integer  novr(0:nphx)	!number of overlap shells for unique pot
      integer  iphovr(novrx,0:nphx) !unique pot for this overlap shell
      integer  nnovr(novrx,0:nphx) !number of atoms in overlap shell

      integer  ifrph(0:nphx)	!given unique pot, which free atom?
      integer  iatph(0:nphx)	!given unique pot, which atom is model?
				!(0 if none specified for this unique pot)

      double precision vtot(mrpts,0:nphx) !overlapped total potential
      double precision rmt(0:nphx)	!muffin tin radius
      double precision rnrm(0:nphx)	!norman radius
      double precision edens(mrpts,0:nphx) !overlapped density*4*pi
      double precision vclap(mrpts,0:nphx) !overlapped coul pot
      double precision folp(0:nphx)	!overlap factor for rmt calculation
      double precision rovr(novrx,0:nphx) !r for overlap shell

      integer  iphat(natx)	!given specific atom, which unique pot?
      double precision rat(3,natx)	!cartesian coords of specific atom

      double precision  ri(mrpts), vtotph(mrpts), rhoph(mrpts)


      complex*16 eref(nex)		!interstitial energy ref
      complex*16 ph(nex,ltot+1,0:nphx)	!phase shifts
      integer    lmax(0:nphx)    !number of ang mom levels

      integer ionin
      integer nr, ne, nemax, nat, ik0, iph, intclc, ixanes, iprint
      character*6 potlbl(0:nphx)

      character*32 words(mwords), key
      character*256 title
      integer natoms, ipot(natx), iatnum(natx), izpot(0:nphx)
      double precision xat(natx), yat(natx), zat(natx)
      double precision gamach, etfin, et
      external istrln
c
      intclc  = 0
      iprint  = 0
      ixanes  = 0
      debug = istat.eq.1
      if (istrln(potfile) .le. 1) potfile = 'potentials.bin'

c read XYZ geometry file
      natoms = natx
      call ReadXYZ(geomfile, natx, nphx, iatnum, ipot, 
     $     xat, yat, zat, natoms, izpot, tmpstr, title, jstat)
      if (jstat.ne.0) then 
         istat = 1
         return
      endif
c
      print*, ' Pot ReadXYZ done ', natoms, nphx
      nwords = mwords
      call strsplit(tmpstr,nwords,words,'$')
      nph = -1
      do i = 0, nphx
         if (izpot(i).ge.1) then 
            nph = i
            potlbl(i) = words(i+1)
            print*, i, izpot(i), potlbl(i)
         endif
      enddo

c check which atom is Central Atom
      ipot0 = -1
      do 100 i = 1, natoms
         if (ipot(i) .eq.0) ipot0 = i
 100  continue 
      if (ipot0.eq.-1) then 
         istat = 1
         write(tmpstr,'(3a)')
     $        "Error: No central atom (IPOT=0) seen in '",
     $        geomfile(1:iflen), "'"
         call echo(tmpstr)
         return
      endif

c
c  get core-hole width
      call setgam (iatnum(ipot0), ihole, gamach)

c
c  get free atom potentials and densities
c     NB iwigner is needed in SUMAX, if changed here, change it there
      print*, 'number of potentials: ', nph
      do 20 iph = 0, nph
         itmp = 0
         if (iph .eq. 0)  itmp = ihole
         ionin = 0

         write(tmpstr,'(a,1x,i3)')
     $     'free atom potential and density for atom type', iph
         call echo(tmpstr)
         call atom(potlbl(iph), iph, izpot(iph), itmp, 
     $        ionin, vcoul(1,iph), rho(1,iph), dgc0, dpc0, et)
         
         if (iph .eq. 0)  etfin = et
ccc         print*,  iph, izpot(iph), itmp, et, rho(1,iph), rho(2,iph)
 20   continue
cc
cc  works to here
c
c     Overlap potentials and densitites
      print*, ' Overlap Potentials:: ', nph
      do 40  iph = 0, nph
         write(tmpstr,'(a,i3)') 
     1    'overlapped potential and density for unique potential', iph
         call echo(tmpstr)

         call ovrlp (iph, iphat, rat, iatph, ifrph, novr,
     1        iphovr, nnovr, rovr, izpot, nat, rho, vcoul,
     2        edens, vclap, rnrm)
   40 continue

c     Find muffin tin radii, add gsxc to potentials, and find
c     interstitial parameters
       call echo('    muffin tin radii and interstitial parameters')
       call istprm (nph, nat, iphat, xat, yat, zat, iatph, xnatph,
     1             novr, iphovr, nnovr, rovr, folp, edens,
     2             vclap, vtot, imt, inrm, rmt, rnrm, rhoint,
     3             vint, rs, xf, xmu, rnrmav, intclc)


c     Initialize header routine and write misc.dat
c$$$      call sthead (nhead0, head0, lhead0, nph, iz, rmt, rnrm,
c$$$     1             ion, ifrph, ihole, iexch,
c$$$     2             vrexch, viexch, rsexch, gamach, xmu, xf, vint, rs,
c$$$     3             nhead, lhead, head)
c$$$      if (iprint .ge. 1)  then
c$$$         open (unit=1, file='misc.dat', status='unknown', iostat=ios)
c$$$         call chopen (ios, 'misc.dat', 'potph')
c$$$         call wthead(1)
c$$$         close (unit=1)
c$$$      endif
c$$$
c$$$      if (iprint .ge. 2)  then
c$$$         call wpot (nph, edens, ifrph, imt, inrm,
c$$$     1              rho, vclap, vcoul, vtot)
c$$$      endif

c     Phase shift calculation
c     Make energy mesh and position grid
      nr = 250
      dx = .05d0
      x0 = 8.8d0
      edge = xmu - vrexch
      call phmesh (nr, dx, x0, nemax, iprint,
     1             ixanes, edge, xmu, vint, vrexch,
     1             imt, edens, nph,
     2             ri, ne, em, ik0)

c     Cross section calculation, use phase mesh for now
c     remove xanes calculation in feff6l

      do 60  iph = 0, nph
         write(tmpstr,'(a,i3)')
     $        'phase shifts for unique potential', iph
         call echo(tmpstr)
c        fix up variable for phase
         call fixvar (rmt(iph), edens(1,iph), vtot(1,iph),
     1                vint, rhoint, nr, dx, x0, ri,
     2                vtotph, rhoph)

         call phase (iph, nr, dx, x0, ri, ne, em, edge,
     1               iexch, rmt(iph), xmu, viexch, rsexch, gamach,
     2               vtotph, rhoph,
     3               eref, ph(1,1,iph), lmax(iph))
   60 continue

c     Write out phases for genfmt
c     May need stuff for use with headers only
c$$$      open (unit=1, file='phase.bin', access='sequential',
c$$$     1      form='unformatted', status='unknown', iostat=ios)
c$$$      call chopen (ios, 'phase.bin', 'potph')
c$$$      write(1) nhead
c$$$      do 62  i = 1, nhead
c$$$         write(1) head(i)
c$$$         write(1) lhead(i)
c$$$   62 continue
c$$$      write(1) ne, nph, ihole, rnrmav, xmu, edge, ik0
c$$$      write(1) (em(ie),ie=1,ne)
c$$$      write(1) (eref(ie),ie=1,ne)
c$$$      do 80  iph = 0, nph
c$$$         write(1) lmax(iph), iz(ifrph(iph))
c$$$         write(1) potlbl(iph)
c$$$         do 70  ie = 1, ne
c$$$            write(1)  (ph(ie,ll,iph), ll=1,lmax(iph)+1)
c$$$   70    continue
c$$$   80 continue
c$$$      close (unit=1)


c
c done!
      print*, 'Potentials done.  ', potfile(1:istrln(potfile))
      print*, '   Edge: ', ihole, ipot0, iatnum(ipot0), gamach
      print*, '   Exchange: ', iexch, vrexch, viexch, rsexch
      
      
      return 
      end
