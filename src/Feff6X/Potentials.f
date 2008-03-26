       subroutine Potentials(geomfile, potfile,
     $     iedge, iexch, viexch, vrexch, rsexch,istat)

c 
c wrapper for Feff's Potentials Calculation
c     
      implicit none
      include 'dim.h'

      character*(*)     geomfile, potfile
      double precision  viexch, vrexch, rsexch
      integer iexch, iedge, istat

      character*1024  line, tmpstr
      integer istrln, ilen, jlen, iflen, iret, i, ierr, ios
      integer mwords, nwords, ititle, jstat, ipot0
      logical debug
      parameter (mwords = 16)
      character*32 words(mwords), key
      character*128 title
      integer natoms, ipot(natx), iatnum(natx)
      double precision xat(natx), yat(natx), zat(natx)
      double precision gamach
      external istrln
c

      debug = istat.eq.1

      if (istrln(potfile) .le. 1) potfile = 'potentials.bin'

      natoms = natx
      call ReadXYZ(geomfile, natx, iatnum, ipot,
     $     xat, yat, zat, natoms, title, jstat)
      
c
      if (jstat.ne.0) then 
         istat = 1
         return
      endif

      ipot0 = -1
      do 100 i = 1, natoms
         if (ipot(i) .eq.0) ipot0 = i
 100  continue 

      if (ipot0.eq.-1) then 
         istat = 1
         return
      endif

c  get core-hole width
      call setgam (iatnum(ipot0), iedge, gamach)

      print*, 'Potentials done.  ', potfile(1:istrln(potfile))
      print*, '   Edge: ', iedge, ipot0, iatnum(ipot0), gamach
      print*, '   Exchange: ', iexch, vrexch, viexch, rsexch
      
      
      return 
      end
