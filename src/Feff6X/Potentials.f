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
      integer mwords, nwords, ititle, jstat, ipot0
      logical debug
      parameter (mwords = 16)
      character*32 words(mwords), key
      character*256 title, potlbl
      integer natoms, ipot(natx), iatnum(natx), izpot(0:nphx)
      double precision xat(natx), yat(natx), zat(natx)
      double precision gamach
      external istrln
c
      debug = istat.eq.1
      if (istrln(potfile) .le. 1) potfile = 'potentials.bin'

c read XYZ geometry file
      natoms = natx
      call ReadXYZ(geomfile, natx, nphx, iatnum, ipot, 
     $     xat, yat, zat, natoms, izpot, potlbl, title, jstat)
      if (jstat.ne.0) then 
         istat = 1
         return
      endif
c
      print*, ' Pot ReadXYZ done'
      print*, potlbl(1:istrln(potlbl))
      print*, izpot


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
c     NB wsatom is needed in SUMAX, if changed here, change it there
      wsatom = 15
c     do not save spinors
      ispinr = 0
      do 20  ifr = 0, nfr
         itmp = 0
         if (ifr .eq. 0)  itmp = ihole
         write(messag,10)
     $        'free atom potential and density for atom type', ifr
         call echo(messag)
         call atom (head0(1)(1:40), ifr, iz(ifr), itmp, wsatom,
     1              ion(ifr), vcoul(1,ifr), rho(1,ifr),
     2              ispinr, dgc0, dpc0, et)
c        etfin is absorbing atom final state total energy
c        etinit is absorbing atom initial state (no hole)
         if (ifr .eq. 0)  etfin = et
   20 continue

c
c done!
      print*, 'Potentials done.  ', potfile(1:istrln(potfile))
      print*, '   Edge: ', ihole, ipot0, iatnum(ipot0), gamach
      print*, '   Exchange: ', iexch, vrexch, viexch, rsexch
      
      
      return 
      end
