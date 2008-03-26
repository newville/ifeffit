      subroutine ReadXYZ(geomfile, natx, iatnum, ipot, x, y, z,
     $     natoms, title, istat)
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
      integer iflen, ios, istrln, iret, iread, natoms, iz, natx
      integer mpots, mwords, nwords, ititle, nat1, iat
      integer ipottmp, istat, ierr, nline, i, j, max_ipot

      character  line*256, tmpstr*1024

      integer iatnum(natx), ipot(natx)
      double precision x(natx), y(natx), z(natx)

      logical is_comment, debug, has_ipot0, re_order_ipots


      parameter (mwords = 8, mpots = 9)

      integer ipot2iz(0:mpots+1)

      character*32 words(mwords), key
     
      external istrln, is_comment

      iflen = istrln(geomfile)

      istat = 0
      open (unit=1, file=geomfile, status='old', iostat=ios)
      if (ios .gt. 0)  then
         istat = ios
         write(tmpstr,'(3a)') "Feff6 cannot open Geometry file '",
     $        geomfile(1:iflen), "'"
         call echo(tmpstr)
         return
      endif 


      if (istat.ne.0) return

c
      do 20 i = 1, natx
         x(i) = 0.d0
         y(i) = 0.d0
         z(i) = 0.d0
         iatnum(i) = -1
         ipot(i)   = -1
 20   continue

      do 25 i = 0, mpots+1
         ipot2iz(i) = -1
 25   continue 
      nline = 0
      iat   = 0
      has_ipot0 = .false.

c     read geometry file
 100  continue
      iret = iread(1, line)
      nline  = nline + 1
      if (nline.eq.1) then 
         call str2in(line,nat1, ierr)
      else if (nline.eq.2) then 
         title = line
         call triml(title)
      else          
         if (iret.eq. 0) goto  100 ! blank line, get next
         if (iret.le.-1) goto  200 ! end of input, finish loop
         call triml(line)
         if (is_comment(line))  goto 100
         nwords = mwords
         do 105 i = 1, nwords
            words(i) = '0'
 105     continue 
         call bwords(line, nwords, words)
         call ReadXYZ_AtNum(words(1), iz)
         if (iz.gt.1) then  ! ignore H!!
            iat = iat + 1
            if (iat.gt.natx) goto 200
            iatnum(iat) = iz
            call str2dp(words(2), x(iat), ierr)
            call str2dp(words(3), y(iat), ierr)
            call str2dp(words(4), z(iat), ierr)
            if (nwords.ge.5) then
               call str2in(words(5), ipot(iat), ierr)
               if (ipot(iat).eq.0) then
                  if (has_ipot0) then
                     write(tmpstr,'(3a)')"Error reading '",
     $     geomfile(1:iflen), "': multiple atoms set to IPOT=0"
                     call echo(tmpstr)
                     istat = 1
                  endif
                  has_ipot0 = .true.
                  ipot2iz(0) = iz

               else if ((ipot(iat).gt.9).or.(ipot(iat).lt.0)) then
                  call echo('Warning: '//
     $                 'IPOT must be in range 0 to 9')
               else
c                 if this ipot has been seen before, check that
c                 it was for the same IZ!!
                  do 110 i = 0, mpots
                     if ((ipot2iz(i) .eq. iz).and.
     $                    (ipot(iat) .ne. i)) then
                        
                        call echo('Warning: '//
     $            'IPOT used for two different atomic numbers')
                     endif
 110              continue 
                  ipot2iz(ipot(iat)) = iz                     
               endif
            endif
         endif
      endif
      goto 100
 200  continue
c   finished reading geometry file
      close(unit=1)
c   on error, return now

      if (istat .ge. 1) return

c  now, clean up the ipot assignments.

c  if IPOT=0 is not set, assign it to the first atom without an IPOT 
      if (.not.has_ipot0) then 
         do 380 i = 1, iat
            if (ipot(i).lt.0) then
               ipot(i) = 0
               ipot2iz(0) = iatnum(i)
               goto 382
            endif
 380     continue 
 382     continue 
      endif


c
c  loop over atoms to assign ipots
c
      do 410 i = 1, iat
c
c   if the ipot is not assigned, loop (while-loop) to find
c   an ipot with matching Z or the next unassigned ipot.
c   not that we start at ipot=1, ignoring ipot 0
         if (ipot(i) .lt. 0) then
            j = 0
 405        continue 
            j = j+1
            if (ipot2iz(j).eq.iatnum(i)) then
               ipot(i) = j
            else if (ipot2iz(j) .lt. 0) then
               ipot(i) = j
               ipot2iz(j) = iatnum(i)
            else if (j.lt.mpots) then
               goto 405
            endif
         endif
 410  continue 

c now check that ipots are sequential, with now breaks
 450  continue 
      max_ipot = 0
      do 480 i = 0, mpots
         if (ipot2iz(i).ge.0) max_ipot = i
 480  continue 

      re_order_ipots = .false.
      do 490 i = 0, max_ipot
         if (ipot2iz(i).lt.0) then 
            ipot2iz(i) = ipot2iz(max_ipot)
            ipot2iz(max_ipot) = -1
            do 485 j = 1, iat
               if (ipot(j) .eq. max_ipot) ipot(j) = i
 485        continue 
            re_order_ipots = .true.
            goto 492
         endif
 490  continue 
 492  continue 
      if (re_order_ipots) goto 450


      print*, ' Final IPOT->IZ mapping'
      do i = 0, mpots
         if (ipot2iz(i).ge.0)  print*, i, ipot2iz(i)
      enddo

      print*, ' Final Atom list:'
      do 500 i = 1, iat
         print*, iatnum(i), ipot(i), x(i),y(i),z(i)
 500  continue 


      return 
      end
      subroutine ReadXYZ_AtNum(s,iz)
c
c  given string with Atomic Number or Atom Symbol, return Atomic Number

      implicit none
      character*(*) s
      integer  iz, ierr, atomic_z
      external atomic_z

      iz = 0
c first test for integer
      call str2in(s,iz,ierr)
      if ((iz.le.0).or.(ierr.ne.0)) then
         iz =atomic_z(s)
      endif

      return
      end
