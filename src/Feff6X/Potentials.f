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


c done!
      print*, 'Potentials done.  ', potfile(1:istrln(potfile))
      print*, '   Edge: ', ihole, ipot0, iatnum(ipot0), gamach
      print*, '   Exchange: ', iexch, vrexch, viexch, rsexch
      
      
      return 
      end
