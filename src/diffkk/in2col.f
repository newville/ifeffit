      subroutine in2col(active,filnam,mdoc,mpts,
     $      ix1,ix2, doc,ndoc,x1,x2,npts,isfeff)
c
c open and reads two columns from a multi-column data file.
c arguments:
c   filnam file name containing data           (in)
c   mdoc   max number of document lines to get (in)
c   mpts   max number of data elements to get  (in)
c   ix1    column to read array x1 from        (in)
c   ix2    column to read array x2 from        (in)
c   doc    array of document lines             (out)
c   ndoc   number of doc lines returned        (out)
c   x1     first array                         (out)
c   x2     second array                        (out)
c   npts   number of data points               (out)
c
       implicit none
       integer   ilen , istrln, j, i, maxwrd, ndoc, npts, iounit
       integer   iexist, ierr, nwords, idoc, id, mdoc,mpts,ncol
       integer   ix1, ix2, ixmax, ixmin
       parameter(maxwrd = 10)
       double precision   x1(mpts),x2(mpts), zero
       parameter(zero = 0)
       logical       isdat, ffhead, adddoc, comm, isfeff, active
       character*(*) filnam, doc(mdoc), commnt*4, ffstr*5,ffend*2
       character*30  words(maxwrd), str*100, file*100
       external      istrln, isdat
       data          commnt, iounit, adddoc /'#*%!', 0, .true./
       data          ffhead, ffstr,  ffend  /.false.,' feff','@#'/

       ncol = 3
       if (active) isfeff = .false.
c
       ixmax = max(ix1,ix2)
       ixmin = min(ix1,ix2)
       if (.not.active.and.
     $      (ixmin.le.0).or.(ixmax.gt.maxwrd)) go to 880
 10    format(a)
       file = filnam
 20    continue 
       ilen = istrln(file)
       if (ilen.le.0)  go to 890
c  open data file
       call openfl(iounit, file, 'old', iexist, ierr)
       if ((iexist.lt.0).or.(ierr.lt.0)) then
          if (.not.active) go to 900
          call messag(' couldn''t find file '//file(:ilen))
          call askstr(' type a new file name (or N to abort)',
     $         file)
          str = file
          call smcase(str,'n')
          if (str.eq.'n')  go to 999
          go to 20
       end if
c  initialize buffers
       do 80 j = 1, mdoc
          doc(j) = ' '
  80   continue
       do 90 j = 1, mpts
          x1(j) = zero
          x2(j) = zero
  90   continue
       do 100 i = 1, maxwrd
          words(i) = ' '
 100   continue
       idoc = 0
       id   = 0
c
c  get documents from header: up to ndoc
 200   continue
          print*, ' line 200', comm, ffhead
          comm = .false.
          read(iounit, 10, end = 920, err = 930) str
          call triml (str)
c  remove leading comments (followed by optional blanks) from str 
          if ( index(commnt, str(1:1)).ne.0 )  then
             comm     = .true.
             str(1:1) = ' '
             call triml(str)
          end if
c  if str is '----', stop adding lines to doc
          if (str(2:4) .eq. '---')  adddoc  = .false.
c  if the str is all numbers and we're not reading a 
c  feff header or a commented out lins, then this is data!
          if ((.not.(comm.or.ffhead)).and.(isdat(str))) goto 410
c  save str in doc if there's room
          ilen = istrln(str)
          if (adddoc .and. (idoc .lt. ndoc) .and. ilen.gt.0)  then
             idoc = idoc + 1
             doc(idoc) = str
          endif
c test for whether reading feff headers (lines may not be commented out,
c but will have '   feff ' in latter part of the first line, and the 
c magic '@#' characters just before numbers -- zany zabinsky!)
          if (idoc.eq.1)  then
             call smcase(str,'a')
             ffhead = (index(str(55:),ffstr).ne.0)
             isfeff = ffhead
             ix2 = 2
             if (ffhead) ix2 = 4
             if (active) then
                str = 'n'
                if (ffhead)  str = 'y'
                call askstr('** is this a feff xmu.dat file?',str)
                call smcase(str,'y')
                ffhead = (str.eq.'y') 
                isfeff = ffhead
                call messag(' ')
                call messag(' What columns are energy and mu(E) in?:')
                if (ffhead) then 
                   ix2 = 4
                call messag(' (Feff puts mu in column 4,'//
     $                  ' mu0 in column 5)')
                end if
 320            continue 
                call askint('** column for energy ',ix1)
                call askint('** column for mu ',ix2)
                ixmax = max(ix1,ix2)
                ixmin = min(ix1,ix2)
                if ((ixmin.le.0).or.(ixmax.gt.maxwrd)) then
                   call messag( '   something''s wrong with '//
     $                  'those values, try again ...')
                   go to 320
                end if
             end if
          end if
c  end first column check if feff xmu / getting of column numbers
          if (ffhead) ffhead = (str(ilen-1:).ne.ffend)
          print*, ' at  goto 200',  active
       
       goto 200
c      
c  read numerical data


cc       print*, ' line 400'
 400   continue
          read(iounit, 10, end = 500, err = 980) str
 410      continue 
          call triml (str)
          if (istrln(str).le.0) go to 400
          if (id.ge.mpts) go to 500
          id = id + 1
          nwords = maxwrd
          call untab(str)
          call bwords(str,nwords,words)
          if (nwords.le.1)      goto 940
          if (nwords.lt.ixmax)  goto 950
          if (id.eq.1) ncol = nwords
          if (ncol.ne.nwords)   goto 960
          call str2dp(words(ix1), x1(id), ierr) 
          if (ierr.ne.0)        goto 990
          call str2dp(words(ix2), x2(id), ierr) 
          if (ierr.ne.0)       goto 990
       goto 400
 500   continue
       npts = id 
       if (idoc.le.0) then
          ndoc   =  1
          doc(1) = 'in2col: no document line found'
       else
          ndoc = idoc
       end if
c      close data file and return
       close(iounit)
cc       print*, 'in2col done'
       return
c      error handling
 880   call messag( ' in2col:  selected columns out of range')
       go to 999
 890   call messag( ' in2col:  no file name given')
       go to 999
 900   call messag( ' in2col: could not find file')
       go to 990
 920   call messag( ' in2col: error reading document lines')
       go to 990
 930   call messag( ' in2col: unexpected end-of-file while '//
     $      'reading titles')
       go to 990
 940   call messag( ' in2col: too few columns of numbers !')
       go to 990
 950   call messag( ' in2col: fewer columns found than requested')
       go to 990
 960   call messag( ' in2col: number of columns changed !')
       go to 990
c      error at reading numerical data
 980   call messag( ' in2col: error reading numerical data.')
 990   call messag( ' in2col: error with file '//file(1:ilen) )
       close(iounit)
 999   continue 
       stop
c end subroutine in2col
       end
