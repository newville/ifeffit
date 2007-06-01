      subroutine outasc(filnam, title, doc, ndoc,
     $      xd, mcol, mdata, ncol, ndata)
c
c write column data to ascii file
c inputs (no outputs):
c   filnam    file name to write data to
c   title     string to use as label for columns
c   doc       array of document lines
c   mdoc      dimension of doc
c   xd        (mcol,mdata) matrix of data
c   mcol      dimension of xd
c   mdata     dimension of xd
c   ncol      number of columns to actually write
c   ndata     number of data points to actually write
       implicit none
       integer  ilen, istrln, i, iounit, ierr, mcol, mdata
       integer  ndoc, mxl, ncol, ndata, iexist, j, ic
       double precision    xd(mcol,mdata)
       character*(*) filnam, title, lines*20
       character*(*) doc(ndoc)
       character     fmt*18, cmt*2, cmtd*2, contc*5
       parameter(mxl = 78, cmtd = '# ', contc = '  +  ')
       parameter(lines = '--------------------')
       external istrln 
c
c decide comment character
       cmt = cmtd
       ic  = istrln(cmt)
c open data file
c if file name is ' ' or '*', write to standard output (unit 6)
       iounit = 6
       if ((filnam.ne.' ').and.(filnam.ne.'*')) then
          iounit = 0
          call openfl(iounit, filnam, 'unknown', iexist, ierr)
          if ((ierr.lt.0).or.(iexist.lt.0)) go to 990
       endif
c write documents
       do 200 i = 1, ndoc
          call triml(doc(i))
          ilen = istrln(doc(i))
          if (ilen.gt.mxl) then
             write(iounit,820) cmt,doc(i)(1:mxl)
             write(iounit,820) cmt//contc,doc(i)(mxl+1:ilen)
          else if (ilen.ge.1) then
             write(iounit,820) cmt,doc(i)(1:ilen)
          end if
 200   continue
c  write line of minus signs and column label
       write(iounit,820) cmt(1:ic), lines//lines
       ilen = max(1,istrln(title))
       write(iounit,820) cmt,title(1:ilen)
c determine format
       write(fmt,850) ncol
c write out column data
       do 400 i = 1, ndata
          write(iounit,fmt) (xd(j,i), j= 1,ncol)
 400   continue
c  close data file and return
       close(iounit)
       return
 820   format(2a)
 830   format(a,40('-'))
 850   format('(',i2,'(g15.7))')
 990   continue
       fmt  = 'outasc: error opening file '//filnam
       ilen = istrln(fmt)
       call messag(fmt(:ilen))
       stop
c end subroutine outasc
       end
