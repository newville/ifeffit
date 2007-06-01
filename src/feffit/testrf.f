       subroutine testrf(flnam, irecl, flform, ier)
c
c   test whether a data file can be interpreted as  uwxafs binary
c   data file or  ascii column data file.
c
c   uwxafs binary files use direct access binary files
c   with word size irecl, which is a machine dependent parameter
c
c ier = -1 : file not found
c ier = -2 : broken uwxafs file?
c ier = -3 : not uwxafs file, but can't find data.
c ier = -4 : looks like ascii, saw line  of minus signs,
c             but 2nd following line doesn't have data
c
c   copright 1994 university of washington   matt newville
c -----------------------------------------------------
      integer   i, irecl, iunit
      character*(*) flnam, flform, line*128
      integer*2    indx(4)
      logical    exist, opend, isdat, prevdt, lisdat
      external  isdat
c -----------------------------------------------------
      flform = 'none'
      ier    = -1
      iunit  = 7
 10   continue
      inquire(unit=iunit, opened = opend)
      if (opend) then
         if (iunit.gt.20) return
         iunit = iunit + 1
         go to 10
      endif
      inquire(file = flnam, exist = exist)
      if (.not.exist) return
      ier    = -2
c -----------------------------------------------------
c try reading file as a uwxafs binary file
c     which have patriotic magic numbers embedded in them
      indx(3) = 0
      indx(4) = 0
      open(iunit, file= flnam, recl = irecl, err = 20,
     $      access = 'direct', status = 'old' )
 20   continue
      read(iunit, rec=1, err = 25) (indx(i), i=1,4)
 25   continue
      if ((indx(3).eq.1776).and.(indx(4).eq.704)) then
         flform = 'uwxafs'
         ier  = 0
         go to 900
      end if
c -----------------------------------------------------
c try to read file as ascii data file
      close(iunit)
      open(iunit, file=flnam, status='old')
      prevdt = .false.
 200  continue
         ier  = -3
         read(iunit, '(a)', end = 900, err = 900) line
         call sclean (line)
         call triml (line)
         if (line(3:6) .eq. '----') then
            ier = -4
            read(iunit, '(a)', end = 900, err = 900) line
            call sclean (line)
            read(iunit, '(a)', end = 900, err = 900) line
            call sclean (line)
            lisdat = isdat(line)
            if (lisdat ) then
               flform = 'ascii'
               ier = 0
            end if
            go to 900
         end if
c if two lines in a row have all words being numbers, it is  data
         lisdat = isdat(line)
         if (lisdat.and.prevdt)  then
            flform = 'ascii'
            ier = 0
            go to 900
         end if
         prevdt = lisdat
         go to 200
c---------------------
 900  continue
      close(iunit)
      return
c end subroutine testrf
      end
