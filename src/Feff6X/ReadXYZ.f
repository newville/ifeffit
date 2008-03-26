      subroutine ReadXYZ(geomfile, natx, iatnum, ipot, x, y, z,
     $     natoms, title)
c
c  Read an XYZ file (or Feff-extended XYZ file) for Feff cluster of atoms
c
c  An XYZ has the following format:
c     line 1:  natoms     (integer number of atoms)
c     line 2:  title      (a title line)
c     lines with 4 columns:
c           AtSym/IZ     x    y    z
c       that is, either an Atomic Symbol (Cu, Zn) or IZ (29, 30)
c       followed by atomic coordinates (in Angstroms unless the units
c       are specified as below).

c  The main issues for Feff are:
c    a) determining the Absorbing Atom
c    b) assigning unique free atom potentials (ipot) for each atom.
c
c  The approaches here are:
c       allow a fourth column detaermining the Absorbing Atom
c 
      implicit none
      character*(*) geomfile, title
      integer iflen, ios, istrln, iret, iread, natoms
      character  line*256, tmpstr*1024

      integer iatnum(natx), ipot(natx)
      double precision x(natx), y(natx), z(natx)
      integer mwords, nwords, ititle, iread, nat1, iat
      integer ipottmp
      logical iscomm, debug
      parameter (mwords = 8)
      character*32 words(mwords), key
     
      external istrln, iscomm

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
c
      do 20 i = 1, natx
         x(i) = 0.d0
         y(i) = 0.d0
         z(i) = 0.d0
         iatnum(i) = -1
         ipot(i)   = -1
 20   continue

      nline = 0
      iat   = 0
      print*, ' reading Geometry file'

o 100  continue
      iret = iread(1, line)
 
      nline  = nline + 1
      if (nline.eq.1) then 
         call str2in(line,nat1, ierr)
      else if (nline.eq.2) then 
         title = line
         call triml(title)
      else 
         
         if (iret.eq. 0) goto  100 ! blank line, get next
         if (iret.le.-1) goto  300 ! end of input, finish loop
         call triml(line)
         if (iscomm(line))  goto 100
         nwords = mwords
         do 105 i = 1, nwords
            words(i) = '0.0'
 105     continue 
         call bwords(line, nwords, words)
         call ReadXYZ_AtNum(words(1), iz)
         if (iz.gt.1) then  ! ignore H!!
            iat = iat + 1
            if (iat.gt.natx) goto 300
            iatnum(iat) = iz
            call str2dp(words(2), x(iat), ierr)
            call str2dp(words(3), y(iat), ierr)
            call str2dp(words(4), z(iat), ierr)
            if (nwords.ge.5) then
               call str2dp(words(5), ipot(iat), ierr)
            endif
            `
         endif

      endif
 300  continue
      
      print*, ' Read XYZ done'


      return 
      end
      subroutine ReadXYZ_AtNum(s,iz)
c
c  given string with Atomic Number or Atom Symbol, return Atomic Number

      character*(*) s
      integer      iz, ierr

      iz = 0
c first test for integer
      call str2in(s,iz,ierr)
      if ((iz.gt.0).and.(ierr.eq.0)) then return   

      call lower(s)

      iz = 3
      return
      end
