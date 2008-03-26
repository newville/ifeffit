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
      integer mwords, nwords, ititle, iread
      logical iscomm, debug
      parameter (mwords = 16)
      character*32 words(mwords), key
      character*128 title
      integer natoms, ipot(natx), iatnum(natx)
      double precision rat(3,natx)

      external istrln, iscomm, iread
c

      debug = istat.eq.1


      print*, 'This is Potentials ', geomfile(1:istrln(geomfile))
      if (istrln(potfile) .le. 1) potfile = 'potentials.bin'

      natoms = natx
      call ReadXYZ(geomfile, natx, ipot, iatnum, rat, natoms,title)

c
      print *, ' Potentials Done'
      return 
      end
