c----------------------------------------------------------------------
c          input/output routines for data files
c               for the uwxafs programs
c
c   input/output routines for data files for the uwxafs programs
c
c   copyright 1992  university of washington, seattle, washington
c   written by      matthew newville
c                   department of physics, fm-15
c                   university of washington
c                   seattle, wa   usa 98195
c   phone           (206) 543-0435
c   e-mail          newville@u.washington.edu
c
c  these routines are the basic input/output routines for getting
c  numerical and document data from files into the uwxafs programs.
c  there are currently two data formats supported:
c
c 1. 'uw' :  a binary file format known as the uwxafs file handling
c            routines. this is very efficient way to store data, and
c            can store several (191) data sets in a single file. the
c            drawback is that the files are not extremely portable.
c
c 2. 'asc':  these are column files in a format that is fairly easy
c            for anything to deal with. the files have several lines
c            of documents. if the first character of the document is
c            '#' this character will be removed. after the documents
c            is a line with minus signs for characters(3:6), then an
c            ignored line (for column labels), and then the data. up
c            to five columns are used. the expected order is:
c                  x, real(y), imag(y), ampl(y), phase(y).
c            if any column representing y is zero, the appropriate
c            value will be calculated and returned. the files in this
c            format hold only one data set, and use more memory than
c            the uwxafs files, but are portable and convenient.
c
c  other file types can be added without too much difficulty.
c  the routines listed here are:
c      inpdat : retrieve data and documents from a file
c      inpcol : retrieve data and documents from an ascii file
c      inpuwx : retrieve data and documents from a uwxafs file
c      outdat : write data and documents to a file
c      outcol : write data and documents to an ascii file
c      outuwx : write data and documents to a uwxafs file
c
c  note: the fortran input/output unit number 11 is used for all
c        unit numbers in these routines. conflicts between these
c        routines will not happen, but conflicts may arise if
c        unit = 11 indicates an open file in a calling subprogram.
c----------------------------------------------------------------------
       subroutine inpdat(filtyp, format, filnam, vax, skey, nkey,
     $      irecl, ndoc, doc, ndata, xdata, yreal, yimag, yampl, yphas)
c
c   copyright 1992  university of washington :          matt newville
c
c    retrieve data and documents from a file acording
c    to the format specified by 'format'.
c inputs:
c   filtyp    file type to open. if may be ' '
c   format    file format (uwxafs, ascii, column)
c   filnam    file name
c   vax       logical flag for being on a vax machine (binary file)
c   skey      symbolic key for record in uwxafs file
c   ndata     maximum number of elements in data arrays
c   nkey      numeric key for record in uwxafs file
c   ndoc      maximum number of document lines to get
c               note:   ndoc cannot be less than or equal to zero!
c outputs:
c   skey      symbolic key of record in uwxafs file
c   ndoc      number of document lines returned
c   doc       array of document lines
c   ndata     number of elements in data arrays
c   xdata     array of x values of data
c   yreal     array of real part of y data values
c   yimag     array of imaginary part of y data values
c   yampl     array of amplitude part of y data values
c   yphas     array of phase part of y data values
c---------------------------------------------------------------------
       implicit none
       character*(*)  filtyp, format, skey, filnam, doc(*)
       character*10   type, symkey, form, formin, errmsg*128
       double precision  xdata(*), yreal(*), yimag(*)
       double precision  yampl(*), yphas(*)
       logical    vax
       integer    irecl, ndatmx, ndocmx, ier, ilen, istrln
       integer    ndata, ndoc, nkey
       external istrln
       data      ndocmx, ndatmx  /19, 4096/
c---------------------------------------------------------------------
c some initializations
       if ((irecl.le.0).or.(irecl.ge.2048)) irecl = 512
       type = filtyp
       call triml(type)
       call upper(type)
       symkey = skey
       call triml(symkey)
       call upper(symkey)
c
c determine format of the input file
       formin = format
       call triml(formin)
       call smcase(formin, 'a')
       call testrf(filnam, irecl, form, ier)
       call smcase(form, 'a')
       if (ier.eq.-1) then
          call echo('  inpdat error: file not found  ')
       elseif (ier.eq.-2) then
          call echo('  inpdat error: unknown file format = '//formin)
       elseif (ier.eq.-3) then
          call echo('  inpdat error: poorly formatted ascii data?  ')
       elseif (ier.eq.-4) then
          call echo('  inpdat error: no data in ascii format file? ')
       end if
       if (ier.ne.0) then
          errmsg =    '    for file ' // filnam
          ilen   = istrln(errmsg)
          call fstop(errmsg(1:ilen) )
       endif
       if ((formin.ne.' ').and.(formin(1:2).ne.form(1:2))) then
          call echo('  inpdat warning: the requested format was'//
     $         ' incorrect!')
          call echo('  form    = '//form(1:5)  )
          call echo('  formin  = '//formin(1:5)  )
       end if
c  now call the appropriate routine to get the data,
c  according to the format.
       ndata = max(1, min(ndata, ndatmx) )
       ndoc  = max(1, min(ndoc , ndocmx) )
cc       print*, 'inpout: ', form(1:2)
       if (form(1:2).eq.'uw') then
          ndoc = ndocmx
          call inpuwx(type, filnam, skey, nkey, irecl, ndoc, doc,
     $         ndata, xdata, yreal, yimag, yampl, yphas )
       elseif ((form(1:2).eq.'co').or.(form(1:2).eq.'as')) then
          call inpcol(filnam, ndoc, doc,
     $         ndata, xdata, yreal, yimag, yampl, yphas )
          skey   = 'ascii'
          call upper(skey)
       else
          call echo('  inpdat error: unknown file format = '// form)
          ilen   = min(54, max(1, istrln(filnam)))
          errmsg = '                for file ' // filnam(1:ilen)
          call fstop( errmsg(1:ilen+26) )
       end if
       filtyp = type
       format = form
c
       return
c end subroutine inpdat
       end
       subroutine inpcol(filnam, ndoc, doc, ndata,
     $                   xdata, yreal, yimag, yampl, yphas)
c
c   copyright 1992  university of washington :       matt newville
c
c   open and get all information from a column file. document
c   lines are read until a line of '----', then a label line is
c   skipped and the column data are read in.  the data is read
c   and stored in the following order:
c                xdata  yreal  yimag  yampl  yphas
c inputs:
c   filnam    file name containing data
c   ndoc      maximum number of document lines to get
c   ndata     maximum number of elements in data arrays
c outputs:
c   ndoc      number of document lines returned
c   doc       array of document lines
c   ndata     number of elements in data arrays
c   xdata     array of x values of data
c   yreal     array of real part of y data values
c   yimag     array of imaginary part of y data values
c   yampl     array of amplitude part of y data values
c   yphas     array of phase part of y data values
c---------------------------------------------------------------------
       implicit none
       integer   ilen , istrln, j, i, mxword, ndoc, ndata, iounit
       integer   iexist, ierr, nwords, idoc, id
       double precision  zero 
       parameter( zero = 0.d0, mxword = 5)
       double precision  xdata(*), yreal(*), yimag(*)
       double precision  xinp(mxword), yampl(*), yphas(*)
       logical   isdat
       character*(*) filnam, doc(*)
       character*32  words(mxword), line*128, status*10, file*128
       external      istrln, isdat
c---------------------------------------------------------------------
 10    format(a)
       file = filnam
       ilen = istrln(file)
       if (ilen.le.0)  then
           call fstop( ' inpcol:  no file name given')
       end if
c  initialize buffers
       do 80 j = 1, ndoc
          doc(j) = ' '
  80   continue
       do 100 i = 1, mxword
          words(i) = '0.'
          xinp(i)  = zero
 100   continue
       do 120 j = 1, ndata
          xdata(j) = zero
          yreal(j) = zero
          yimag(j) = zero
          yampl(j) = zero
          yphas(j) = zero
 120   continue
c  open data file
      iounit = 7
      status ='old'
      call openfl(iounit, filnam, status, iexist, ierr)
      if ((iexist.lt.0).or.(ierr.lt.0)) go to 900
c
c  get documents from header: up to ndoc
c       read file header, save as document lines,
c       remove leading '#' and '%' both of which are
c       known to be extraneous comment characters.
       nwords = 5
       idoc = 0
       id   = 1
 200   continue
          read(iounit, 10, end = 950, err = 960) line
          call sclean(line)
          call triml (line)
c  if line is '----', read one more line, go read numerical data
          if (line(3:6) .eq. '----')  then
             read(iounit, 10, end = 950, err = 960) line
             call sclean(line)
             goto 400
          end if
c  remove leading '#' or '%' from line
          if ( (line(1:1).eq.'#').or.(line(1:1).eq.'%') ) then
             line(1:1) = ' '
             call triml(line)
c  if the line is all numbers, then this is data!
          elseif (isdat(line)) then
             goto 410
          end if
c  save line in doc if there's room
          if ((idoc .lt. ndoc) .and. (istrln(line).gt.0) ) then
             idoc = idoc + 1
             doc(idoc) = line
          endif
          goto 200
c
c  read numerical data
 400   continue
          nwords = 5
          read(iounit, 10, end = 600, err = 980) line
          call sclean(line)
 410      continue
          call untab(line)
          call bwords(line,nwords,words)
          if (nwords.le.1) goto 600
          do 450 i = 1, nwords
              call str2dp(words(i), xinp(i), ierr)
              if (ierr.ne.0) goto 600
 450      continue
          xdata(id) = xinp(1)
          yreal(id) = xinp(2)
          yimag(id) = xinp(3)
          yampl(id) = xinp(4)
          yphas(id) = xinp(5)
          if (id.ge.ndata) go to 610
          id = id + 1
          goto 400
 600   continue
       id    = id - 1
       if (id.lt.1) go to 950
 610   continue
       ndata = id
       if (idoc.le.0) then
          ndoc =  1
          doc(1) = 'inpdat: no document line found'
       else
          ndoc = idoc
       end if
c  make sure that all columns are filled:
c   if yampl and yphas are both zero, compute them from yreal, yimag
c   if yreal and yimag are both zero, compute them from yampl, yphas
       do 800 i = 1, ndata
          if ( ( (yampl(i).eq.zero).and.(yphas(i).eq.zero) ) .and.
     $         ( (yreal(i).ne.zero).or. (yimag(i).ne.zero) ) ) then
            yampl(i) = sqrt( yreal(i)**2 + yimag(i)**2 )
            yphas(i) = atan2( yimag(i), yreal(i) )
             if (i.gt.1) call pijump( yphas(i), yphas(i-1) )

          elseif ( (yreal(i).eq.zero).and.(yimag(i).eq.zero)
     $        .and.(yampl(i).ne.zero)   ) then
            yreal(i) = yampl(i) * cos ( yphas(i) )
            yimag(i) = yampl(i) * sin ( yphas(i) )

          end if
 800   continue
c          print*, ' inpout:'
c       do i = 1, 4
c          print*, xdata(i), yreal(i)
c       end do
c  close data file and return
       close(iounit)
       return
c error handling
c  open file - error
 900   continue
         call echo(' inpcol: error opening file '//file(1:ilen) )
         go to 990
c  end or error at reading documents
 950   continue
 960   continue
         call echo( ' inpcol: error reading file '//file(1:ilen) )
         call echo('         during reading of documents.')
         go to 990
c  error at reading numerical data
 980   continue
         call echo( ' inpcol: error reading file '//file(1:ilen) )
         call echo('         during reading of numerical data.')

 990     continue
         close(iounit)
         stop
c end error handling
c end subroutine inpcol
       end
       subroutine inpuwx(ftypin, filein, skey, nkey, irecl, ndoc,
     $           documt, ndata, xdata, yreal, yimag, yampl, yphas )
c
c   copyright 1992  university of washington :          matt newville
c
c     open and get all information from a uwxafs file
c
c inputs:
c   ftypin   file type to open, checked for compatibility, may be ' '
c   filein   file name containing data
c   skey     symbolic key for record in data file (only one of these)
c   nkey     numeric key for record in data file  (two is needed    )
c   ndoc     maximum number of document lines to get
c outputs:
c   skey      symbolic key of record in uwxafs file
c   ndoc      number of document lines returned
c   docu      array of document lines
c   ndata     number of elements in data arrays
c   xdata     array of x values of data
c   yreal     array of real part of y data values
c   yimag     array of imaginary part of y data values
c   yampl     array of amplitude part of y data values
c   yphas     array of phase part of y data values
c
c notes:
c  1   the full 'noabort' error checking is done for the calls to
c      the uwxafs routines, which means that marginally useful
c      error messages will be given when one of the uwxafs
c      filehandling routines dies.
c
c  2    currently, the following file types are supported:
c           xmu,  chi,  rsp,  env,  rspep, rip
c
c  3    uwxafs file handling routines only do single precision.
c       this routine can be made implicit double precision if the
c       array buffer is maintained as single precision:
c           implicit double precision(a-h,o-z)
c           real           buffer(maxpts)
c---------------------------------------------------------------------
       implicit none
       integer maxpts, ilen, i, ndata, iounit, irecl, istrln
       integer ier, nie, nkey, ndocln, ndoc, ndsent, nbuff, maxdoc
       double precision zero
       parameter( maxpts = 2048, zero = 0.d0 , maxdoc=20)
       character*(*)  ftypin, skey, filein, documt(*)
       character*10   type, ftype, safefl*8, abrtfl*8
       character*128  filnam, messg
       character*100  docbuf(maxdoc)

       double precision xdata(*), yreal(*), yimag(*)
       double precision yampl(*), yphas(*)
       real           buffer(maxpts)
       external   istrln
c---------------------------------------------------------------------
c initialize
 10    format(a)
 20    format(2x,2a)
 30    format(2x,a,i3)
       safefl = ' '
       abrtfl = 'noabort'
       call upper(abrtfl)

       ftype = ftypin
       filnam= filein

       call upper(skey)
       call triml(skey)
       call triml(ftype)
       call triml(filnam)
       ilen = max(1, istrln(filnam))
c note: uwxafs requires ftype to be upper case.
       call upper (ftype)
        do 100 i = 1,ndata
            xdata(i)  = zero
            yreal(i)  = zero
            yimag(i)  = zero
            yampl(i)  = zero
            yphas(i)  = zero
100    continue
       do 110 i = 1, maxpts
            buffer(i) = zero
110    continue
c  call uwxafs file handling routines:
c : open data file
       iounit = 11
       call openrf(iounit, filnam, abrtfl, safefl, ftype, irecl, ier)
       if (ier.ne.0) then
          messg = 'inpuwx: error opening file '
          call echo(messg//filnam(:ilen))
          write (messg, '(9x,a,i4)') 'openrf error code ',ier
          call fstop(messg)
       end if
c : check file type
       call gftype(iounit, type, ier)
       if (ier.ne.0) then
          messg = 'inpuwx: error getting file type for '
          call echo(messg//filnam(:ilen))
          write(messg, '(9x,a,i4)') 'gftype error code ',ier
          call fstop(messg)
       end if
       call upper(type)

       if (ftype.eq.' ') then
           ftype = type
       elseif (ftype.ne.type) then
          messg = 'inpuwx: incorrect file type for '
          call echo(messg//filnam(:ilen))
          messg = '     file type for this file is '
          call echo(messg//type)
          messg = '     file type requested was '
          call fstop(messg//ftype)
       endif
       ftypin = ftype

c : find out how many records there are in the file
       call gnie (iounit, nie, ier)
       if (nie.le.0) then
          messg = 'inpuwx:  no data records in '
          call fstop(messg//filnam(:ilen) )
       end if
c : get skey if it wasn't given as input
       if (skey.eq.' ') then
           call gskey(iounit, nkey, skey, ier)
           if (ier.ne.0) then
              messg = 'inpuwx: error getting skey for '
              call echo(messg//filnam(:ilen))
              write (messg, '(9x,a,i4)') 'gskey error code ',ier
              call fstop(messg)
           end if
           if (skey.eq.' ') then
             write (messg, '(1x,2a,i4)') 'inpuwx: found no skey ',
     $                                  'for nkey =',nkey
             call echo(messg)
             call fstop('        in file = '//filnam(:ilen))
           end if
       end if

c : get nkey if it wasn't given as input
       if (nkey.eq.0) then
           call gnkey(iounit, skey, nkey, ier)
           if (ier.ne.0) then
              messg = 'inpuwx: error getting nkey for '
              call echo(messg//filnam(:ilen))
              write (messg, '(9x,a,i4)') 'gnkey error code ',ier
              call fstop(messg)
           end if
       end if
c
c : get documents : up to ndoc
c   first check how many document lines there are
       call gdlen(iounit, nkey, ndocln, ier)
       if (ier.ne.0) then
          messg = 'inpuwx: error getting document length for '
          call echo(messg//filnam(:ilen))
          write (messg, '(9x,a,i4)') 'gdlen error code ',ier
          call fstop(messg)
       end if
       if (ndoc.gt.ndocln) ndoc = ndocln
c   then get the documents
       call getdoc(iounit, docbuf, ndoc, skey, nkey, ndsent, ier)
       do 300 i = 1, ndsent
          documt(i) = docbuf(i)
 300   continue 
       if (ier.eq.6) then
          messg = 'inpuwx error: reading file '
          call echo(messg//filnam(:ilen) )
          messg = '  no skey or nkey given to specify record, '
          call echo(messg)
          messg = '  or an incorrect skey or nkey given '
          call fstop(messg)
       elseif (ier.ne.0) then
          messg = 'inpuwx: error getting documents for '
          call echo(messg//filnam(:ilen))
          write (messg, '(9x,a,i4)') 'getdoc error code ',ier
          call fstop(messg)
       end if
       ndoc = ndsent

c : get data
       call getrec(iounit, buffer, maxpts, skey, nkey, nbuff, ier)
       if (ier.ne.0) then
          messg = 'inpuwx: error getting data for '
          call echo(messg//filnam(:ilen))
          write (messg, '(9x,a,i4)') 'getrec error code ',ier
          call fstop(messg)
       end if

c : close file
       call closrf(iounit,ier)
       if (ier.ne.0) then
          messg = 'inpuwx: error closing data file '
          call echo(messg//filnam(:ilen))
          write (messg, '(9x,a,i4)') 'closrf error code ',ier
          call fstop(messg)
       end if
c-----------------------------------------------------------------
c finished with uwxafs routines, so now sort the data into
c xdata, re(y), imag(y), ampl(y), phase(y) according to file type
c
c convert ftype to the case of this routine.
c   'case' controls the the case of this routine
       call smcase (ftype, 'case')
c- xmu: nbuff energy, then nbuff y-values
       if (ftype.eq.'xmu') then
            ndata   = nbuff/2
            do 400 i = 1, ndata
               xdata(i) = buffer(i)
               yreal(i) = buffer(ndata + i)
               yampl(i) = yreal(i)
400         continue
c-  chi: xmin, deltax, chi(kmin + i*deltak)
       elseif (ftype.eq.'chi') then
            ndata   = nbuff - 2
            do 500 i = 1, ndata
               xdata(i) = buffer(1) + (i-1)*buffer(2)
               yreal(i) = buffer(2 + i)
               yampl(i) = yreal(i)
500         continue
c-  env,rspep: kmin, deltak, phase, amplitude pairs (kmin + i*deltak)
       elseif ( (ftype.eq.'env').or.(ftype.eq.'rspep')  ) then
            ndata   = (nbuff - 1) / 2
            do 600 i = 1, ndata
               xdata(i) = buffer(1) +(i-1)*buffer(2)
               yphas(i) = buffer(2*i+1)
               yampl(i) = buffer(2*i+2)
               yreal(i) = yampl(i) * cos ( yphas(i) )
               yimag(i) = yampl(i) * sin ( yphas(i) )
600         continue
c  rsp, rip: kmin, deltak, real, imaginary pairs (kmin + i*deltak)
       elseif ( (ftype.eq.'rsp').or.(ftype.eq.'rip')  ) then
            ndata   = (nbuff - 1) / 2
            do 700 i = 1, ndata
               xdata(i) = buffer(1) +(i-1)*buffer(2)
               yreal(i) = buffer(2*i+1)
               yimag(i) = buffer(2*i+2)
               yampl(i) = sqrt( yreal(i)**2 + yimag(i)**2 )
               yphas(i) = atan2( yimag(i), yreal(i) )
                 if (i.gt.1) call pijump( yphas(i), yphas(i-1) )
700         continue
       else
          messg = 'inpuwx: unrecognized file type for '
          call echo(messg//filnam(:ilen))
          messg = '        file type for this file is '
          call fstop(messg//ftype)
       end if
       return
c end subroutine inpuwx
       end
       subroutine outdat(filtyp, format, filnam, vax,
     $     comm, skey, nkey, irecl, ndoc, ndocx, doc, 
     $     ndata, xdata, yreal, yimag, yampl, yphas, iexist)
c
c   copyright 1992  university of washington :          matt newville
c
c    write data and documents to a file acording to the
c    format specified
c inputs:
c   filtyp    file type to open, may be ' '.
c   format    file format (uwxafs, ascii, column)
c   filnam    file name
c   vax       logical flag for being on a vax machine (binary file)
c   comm      comment character for ascii output files
c   ndoc      number of document lines to write
c   ndocx     if non-zero, ndocx  document lines will be written
c               to ascii files, even if "blank" lines are needed.
c   doc       array of document lines
c   ndata     number of elements in data arrays
c   xdata     array of x values of data
c   yreal     array of real part of y data values
c   yimag     array of imaginary part of y data values
c   yampl     array of amplitude part of y data values
c   yphas     array of phase part of y data values
c   iexist    flag for whether to write redundant data to uwxafs file
c             iexist = 1 : do not write redundant data
c             iexist = 0 : do write redundant data
c outputs:
c   skey      symbolic key for record in uwxafs file
c   nkey      numeric key for record in uwxafs file
c   ndoc      number of document lines written
c
c---------------------------------------------------------------------
       implicit none
       integer ndoc, ndocx, ndata, iexist, ilen, nkey, idoc, irecl
       character*(*)  filtyp, format, filnam, skey, doc(*), comm
       character*32   type, form
       double precision xdata(*), yreal(*), yimag(*)
       double precision yampl(*), yphas(*)
       logical        vax
c---------------------------------------------------------------------
       if ((irecl.le.0).or.(irecl.ge.2048)) irecl = 512
c
       idoc = ndoc
       skey = ' '
       form = format
       type = filtyp
       call upper(type)
       call triml(type)
       call triml(form)
c convert form to the case of this routine.
c   'case' controls the the case of this routine
cc       call testrf(filnam, irecl, form, ier)
       call smcase (form, 'a')
c
       if (form(1:2).eq.'uw') then
          if ( (idoc.le.0).or.(idoc.gt.19) )  idoc = 19
          call outuwx(type, filnam, skey, nkey, irecl, idoc, doc,
     $         ndata, xdata, yreal, yimag, yampl, yphas, iexist)
       elseif ( (form(1:3).eq.'col').or.(form(1:3).eq.'asc') ) then
          call outcol(type, filnam, comm, idoc, ndocx, doc,
     $         ndata, xdata, yreal, yimag, yampl, yphas)
          skey = 'ascii'
       else
          call fstop('outdat: unknown file format = '//form)
       end if
c
       return
c end subroutine outdat
       end
       subroutine outcol(filtyp, filnam, comm, ndoc, ndocx, doc, ndata,
     $                  xdata, yreal, yimag, yampl, yphas)
c
c   copyright 1992  university of washington :          matt newville
c
c  open and write all information to a column file. document lines are
c  written, followed by a line of '----', then a label line, and then
c  the data are written.  the file type tells what to use for the label
c  and how many columns to write. it may be left blank.
c
c inputs:
c   filtyp    file type to write (may be ' ' : used for label only)
c   filnam    file name to write (' ' and '*' mean write to unit 6)
c   comm      comment character to specify title lines (up to char*2)
c   ndoc      maximum number of document lines to write
c   doc       array of document lines
c   ndocx     if non-zero, exactly ndocx doc lines will be written
c   ndata     number of elements in data arrays
c   xdata     array of x values of data
c   yreal     array of real part of y data values
c   yimag     array of imaginary part of y data values
c   yampl     array of amplitude part of y data values
c   yphas     array of phase part of y data values
c outputs:
c   ndoc      number of document lines written
c---------------------------------------------------------------------
       implicit none
       integer ndoc, ndocx, ndata, ilen, nkey, idoc, jdoc, i, istrln
       integer mxl, mxlp1, ixmsg, ierr, iexist, iounit, imsg
       double precision zero, xdata(*), yreal(*), yimag(*)
       double precision yampl(*), yphas(*)
       parameter (zero = 0.d0, mxl   = 76)
       character*(*)  filtyp, filnam, doc(*), comm
       character*80   filout, errmsg
       character*35   xmutit, chitit, lines, blank
       character*42   envt1, envt2*32, rspt1, rspt2*32, xyt1,xyt2*32
       character*10   type, status, cmt*2, cmtdef*2,contc*5
       parameter (cmtdef = '# ', contc = '  +  ')
       parameter (lines  ='-----------------------------------')
       parameter (blank  ='    empty comment line')
       parameter (xmutit ='    energy          xmu')
       parameter (chitit ='    k              chi(k)')
       parameter (envt1  ='    k          real[chi(k)]   imag[chi(k)]')
       parameter (envt2  ='   ampl[chi(k)]   phase[chi(k)]')
       parameter (rspt1  ='    r          real[chi(r)]   imag[chi(r)]')
       parameter (rspt2  ='   ampl[chi(r)]   phase[chi(r)]')
       parameter (xyt1   ='    x          real[y(x)]     imag[y(x)]')
       parameter (xyt2   ='   ampl[y(x)]     phase[y(x)]')
       external istrln
c---------------------------------------------------------------------
 20    format(2a)
 30    format(3a)
       type   = filtyp
       call triml(type)
c convert type to the case of this routine.
       call smcase(type, 'a')
       filout = filnam
       call triml(filout)
       if (ndata.le.0) ndata = 2
       if ((ndocx.gt.0).and.(ndocx.lt.ndoc))  ndoc = ndocx
c decide comment character
       cmt  = comm
       if ((cmt.eq.'  ').or.(istrln(cmt).le.0)) cmt = cmtdef
c open data file
c     if file name is ' ' or '*', write to standard output (unit 6)
       iounit = 6
       if ((filout.ne.' ').and.(filout.ne.'*')) then
          iounit = 0
          status ='unknown'
          call openfl(iounit, filout, status, iexist, ierr)
          if ((ierr.lt.0).or.(iexist.lt.0)) go to 990
       endif
c
c write documents
       jdoc  = 0
       mxlp1 = mxl + 1
       do 200 idoc = 1, ndoc
          call triml(doc(idoc))
          ilen = istrln(doc(idoc))
          if (ilen.ge.1) then
             jdoc = jdoc + 1
             if (ilen.gt.mxl) then
                write(iounit, 20) cmt,doc(idoc)(1:mxl)
                write(iounit, 30) cmt,contc,doc(idoc)(mxlp1:ilen)
             else
                write(iounit, 20) cmt,doc(idoc)(1:ilen)
             end if
          elseif (ndocx.gt.0) then
             jdoc = jdoc + 1
             write(iounit, 20) cmt, blank
          end if
 200   continue
       if (ndocx.gt.ndoc) then
          do 210 idoc = ndoc+1,ndocx
             jdoc = jdoc + 1
             write(iounit, 20) cmt, blank
 210      continue
       endif
       ndoc = jdoc
c
c  write line of minus signs and column label
       write(iounit, 30) cmt,lines,lines
       if (type.eq.'xmu') then
          write(iounit, 20) cmt,xmutit
       elseif (type.eq.'chi') then
          write(iounit, 20) cmt,chitit
       elseif (type.eq.'env') then
          write(iounit, 30) cmt,envt1,envt2
       elseif (type.eq.'rsp') then
          write(iounit, 30) cmt,rspt1,rspt2
       else
          write(iounit, 30) cmt,xyt1,xyt2
       end if
c
c  write data: some file types only write out a few columns
       if ( (type.eq.'xmu').or.(type.eq.'chi') ) then
          do 400 i = 1, ndata
             if ((yreal(i).eq.zero).and.(yampl(i).ne.zero))
     $            yreal(i) = yampl(i) * cos(yphas(i))
             write(iounit, 520) xdata(i), yreal(i)
 400      continue
       else
          do 450 i = 1, ndata
c make sure that all of re(y), im(y), amp(y), and phase(y) are known
             if ( ((yampl(i).eq.zero).and.(yphas(i).eq.zero)) .and.
     $            ((yreal(i).ne.zero).or. (yimag(i).ne.zero)) ) then
                yampl(i) = sqrt( yreal(i)**2 + yimag(i)**2 )
                yphas(i) = atan2( yimag(i), yreal(i) )
                if (i.gt.1) call pijump( yphas(i), yphas(i-1) )
             elseif ((yreal(i).eq.zero).and.(yimag(i).eq.zero)
     $               .and.(yampl(i).ne.zero) ) then
                yreal(i) = yampl(i) * cos ( yphas(i) )
                yimag(i) = yampl(i) * sin ( yphas(i) )
             end if
             write(iounit, 550) xdata(i), yreal(i), yimag(i),
     $                          yampl(i), yphas(i)
 450      continue
       end if
 520   format(2x,e13.7,3x,e13.7)
 550   format(2x,e13.7,2x,e13.7,2x,e13.7,2x,e13.7,2x,e13.7)
c
c  close data file and return
       close(iounit)
       return
 990   continue
       ilen   = max(1, istrln(filnam))
       errmsg = 'outcol: error opening file '//filnam(:ilen)
       imsg   = istrln(errmsg)
       call fstop(errmsg(:imsg))
c end subroutine outcol
       end
       subroutine outuwx(ftypin, filein, skey, nkey, irecl, ndoc, doc,
     $           ndata, xdata, yreal, yimag, yampl, yphas, iexist)
c
c     write out data and documents to a uwxafs file
c
c inputs:
c   ftypin    file type to write to, may be ' ' if filnam exists.
c   filein    file name to write to
c   skey      symbolic key of record in uwxafs file
c   ndoc      number of document lines returned
c   doc       array of document lines
c   ndata     number of elements in data arrays
c   xdata     array of x values of data
c   yreal     array of real part of y data values
c   yimag     array of imaginary part of y data values
c   yampl     array of amplitude part of y data values
c   yphas     array of phase part of y data values
c   iexist    flag for whether to write redundant data to file
c               iexist = 1 : do not write redundant data
c               iexist = 0 : do write redundant data
c
c   copyright 1992  university of washington :          matt newville
c-----------------------------------------------------------------------
       implicit none
       integer maxpts, maxdoc, nkey, irecl, ndoc, iexist, ndata,idoc
       integer ierr, iounit, ier, i, nbuff, imsg, istrln, ilen
       double precision zero
       parameter(maxpts = 2048, maxdoc = 19, zero = 0.d0)
       character*(*)  filein, ftypin, doc(*), skey
       character*10   skyout, ftype, type, filnam*128, messg*128
       character*100  docout(maxdoc), abrtfl*8, safefl*8
       double precision xdata(*), yreal(*), yimag(*)
       double precision yampl(*), yphas(*)
       real           buffer(maxpts)
c-----------------------------------------------------------------------
c initialize
 10    format(a)
       safefl = ' '
       abrtfl = 'noabort'
       call upper(abrtfl)
       skyout = ' '
       type   = ' '
       filnam = filein
       call triml(filnam)
       ilen   = max(1, istrln(filnam))
       ftype  = ftypin
       call upper(ftype)
       do 60 i = 1, maxdoc
          docout(i) = ' '
 60    continue
c output documents
       idoc = 0
 80    continue
          idoc  = idoc + 1
          if ((idoc.ge.maxdoc).or.(idoc.gt.ndoc)) then
             idoc  = idoc - 1
             go to 100
          end if
          docout(idoc) = doc(idoc)
          call triml(docout(idoc))
          go to 80
100    continue
ccccc       ndoc = idoc
c  open data file to check file type
       iounit = 11
       call openrf(iounit, filnam, abrtfl, safefl, ftype, irecl, ier)
       if (ier.ne.0) then
          messg = 'outuwx: error opening file '//filnam(:ilen)
          imsg  = max(1, istrln(messg))
          call echo(messg(:imsg))
          write(messg, '(9x,a,i3)' ) 'openrf error code ',ier
          call fstop(messg)
       end if

c  check file type
       call gftype(iounit, type, ier)
       call upper(type)
c  if file type was not given, close and the re-open data file
c           with file type just found, so we can write to file
       if (ftype.eq.' ')  then
           ftype = type
           call closrf(iounit,ier)
           call openrf(iounit, filnam, abrtfl, safefl, ftype,irecl,ier)
c  if file type was given but it was wrong, stop
       elseif (ftype.ne.type) then
          messg = 'outuwx: incorrect file type for '
          call echo(messg//filnam(:ilen))
          messg = '        file type for this file is '
          call echo(messg//type)
          messg = '        file type requested was '
          call fstop(messg//ftype)
       endif
c
c  make sure that all of re(y), im(y), amp(y), and phase(y) are known
       do 300 i = 1, ndata
          if ( ( (yampl(i).eq.zero).and.(yphas(i).eq.zero) ) .and.
     $         ( (yreal(i).ne.zero).or. (yimag(i).ne.zero) ) ) then
            yampl(i) = sqrt( yreal(i)**2 + yimag(i)**2 )
            yphas(i) = atan2( yimag(i), yreal(i) )
             if (i.gt.1) call pijump( yphas(i), yphas(i-1) )

          elseif ( (yreal(i).eq.zero).and.(yimag(i).eq.zero)
     $        .and.(yampl(i).ne.zero)   ) then
            yreal(i) = yampl(i) * cos ( yphas(i) )
            yimag(i) = yampl(i) * sin ( yphas(i) )

          end if
300    continue
c
c  put data into a single buffer according to data type
c convert ftype to the case of this routine.
c   'case' controls the the case of this routine
       call smcase(ftype, 'case')
c  usually buffer(1) and buffer(2) are xdata(1) and xdata(2) -xdata(1)
       buffer(1) = xdata(1)
       buffer(2) = xdata(2) - xdata(1)
c   xmu: nbuff energy, then nbuff y-values
       if (ftype.eq.'xmu') then
            nbuff    = 2*ndata
            do 400 i = 1, ndata
               buffer(i)         = xdata(i)
               buffer(ndata + i) = yreal(i)
400         continue
c   chi: kmin, deltak, chi(kmin + i*deltak)
       elseif (ftype.eq.'chi') then
            nbuff     = ndata + 2
            do 500 i  = 1, ndata
               buffer(2 + i)     = yreal(i)
500         continue
c   env: kmin, deltak, phase, amplitude pairs (kmin + i*deltak)
       elseif ( (ftype.eq.'env').or.(ftype.eq.'rspep') ) then
            nbuff     = 2* (ndata + 1)
            do 600 i  = 1, ndata
               buffer(2*i+1)     = yphas(i)
               buffer(2*i+2)     = yampl(i)
600         continue
c   rsp: kmin, deltak, real, imaginary pairs (kmin + i*deltak)
       elseif ( (ftype.eq.'rsp').or.(ftype.eq.'rip') ) then
            nbuff     = 2* (ndata + 1)
            do 700 i = 1, ndata
               buffer(2*i+1)     = yreal(i)
               buffer(2*i+2)     = yimag(i)
700         continue
c   other data types not yet supported
       else
          call fstop('outuwx: not able to decipher ftype ='//ftype)
       end if
c
c  generate skyout for data with hash
       call hash(buffer, nbuff, docout, idoc, skyout)

c  check if this record is already in the file,
c    and decide whether or not to write data and
c    documentation for the record to the file

       call gnkey(iounit, skyout, nkey, ier)
       if ( (iexist.eq.1).and.(nkey.ne.0) ) then
          skey = ' '
       else
          call putrec(iounit, buffer, nbuff, skyout, 0, ier)
          call putdoc(iounit, docout, idoc,  skyout, 0, ier)
          skey = skyout
       end if

       ftypin = ftype
c  close file and leave
       call closrf(iounit, ierr)
       return
c end subroutine outuwx
       end

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
       if ((irecl.le.16).or.(irecl.ge.2048)) irecl = 512

 70    continue 
       indx(3) = 0
       indx(4) = 0
       open(iunit, file= flnam, recl = irecl, err = 80,
     $      access = 'direct', status = 'old' )
 80    continue
       read(iunit, rec=1, err = 85) (indx(i), i=1,4)
 85    continue
       if ((indx(3).eq.1776).and.(indx(4).eq.704)) then
          flform = 'uwxafs'
          ier  = 0
          go to 900
       else 
          irecl = irecl / 2
          if (irecl .gt. 32) then
             close(iunit)
             go to 70
          endif
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
