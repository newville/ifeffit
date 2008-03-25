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

      subroutine ReadXYZ(geomfile, natx, ipot, iatnum, rat,
     $     natoms, title)

      character*(*) geomfile
      integer iflen, ios, istrln, iret, iread
      character  line*256, tmpstr*1024
      
      iflen = istrln(geomfile)

      open (unit=1, file=geomfile, status='old', iostat=ios)
      if (ios .gt. 0)  then
         istat = ios
         write(tmpstr,10) geomfile(1:iflen)
 10      format ("Feff6 cannot open Geometry file '",a, "'")
         call echo(tmpstr)
         return
      endif 
      
c     

      print*, ' reading Geometry file'
 100  continue
      iret = iread(1, line)

      return 
      end
