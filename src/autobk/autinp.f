      subroutine autinp(iinp, ilog, first, domore, dorun)
c
c  read inputs for the autobk program.
c  the input parameters are read using keywords from the input
c  given by iounit. the xmu data file is opened, the pre-edge
c  is removed, and the standard chi data (if used) is read. the
c  routine inpdat is used for all data files, allowing either
c  uwexafs or ascii column data.
c
c  notes
c   1.   the case as the word "case" at the top of the routine sets
c     the case of all character strings in this routine. be careful
c     when editing this file.
c
c   copyright 1992  university of washington :          matt newville
c-----------------------------------------------------------------------
       include 'autobk.h'
c-----------------------------------------------------------------------
c local variables
       integer     mfil, iinp, nline, ilen, istrln, nwords
       integer   ilog, idot, maxwrd
       parameter   (mfil = 10, maxwrd = 20)
       integer     i, iwrds, i2, ix, i3, jinit
       integer     ierr, ier, iw, is, iii, ii, iargc
       character*128 str, string, strdat, messg, logfil, stat*10
       character*128 keywrd, words(maxwrd), wrdsor(maxwrd), key*3
       logical      domore, dorun, errskp, first, exist
       external     istrln, iargc
c---------------------------------------------------------------------
c  initialize local variables
       domore =   .false.
       dorun  =   .false.
       jinit  =   1
       nline  =   1
c
c  if first time through, find a version of the input file
c  and open it:                 check upper and lower case
       if (first) then
          jinit  = -1
          exist  =  .false.
          ier    =  0
          stat   =  'old'
          call get_inpfile('autobk.inp',string,ier)

cc          if (macflg) then
c#mac
c#  the following code can be used with the LS Fortran Compiler
c#  thanks to boyan boyanov for this code
cc             open(unit=iinp,file=*,status='old',iostat=ier)
cc             if (ier.ne.0) then
cc                call AlertBox('File selection was canceled!')
cc                go to 1010
cc             end if
cc             call f_setvolume(jvrefnum(iinp))
cc             call f_creator('ttxt')
cccc this resets fname to the name of the opened file. this may be useful
cccc for computing output file names, etc.
cc             inquire(unit=iinp,name=string,iostat=ier)
cc             if (ier.ne.0) string='autobk.inp'
c#mac
cc          end if

c now open log file 
          stat   =  'unknown'
          logfil =  'autobk.log'
          if (string.ne.'autobk.inp') then
             call triml(string)
             idot  = max(1,istrln(string))
             if (index(string(1:idot),'.').ne.0) then 
 15             continue
                if (index(string(idot:idot),'.').eq.0) then
                   idot = idot - 1
                   go to 15
                end if
             end if
             logfil = string(1:idot) //'log'
          end if
          open(unit=ilog, file=logfil, status=stat, err=1020)
          write(ilog,'(2x,2a)') ' ----------------------- automatic',
     $            ' background removal-----------------------'
          ilen = max(1, istrln(versn))
          write(ilog,'(2x,a)') versn(1:ilen) 
c initialize wfftc array if this the first time through.
c this array will remain unchanged throughout the running
c of the program. note that mftfit = 1024 implies that
c k cannot be larger than 51.2 A^-1, and that the spacing
c between points in r-space will be 0.061A
c
cc          mftfit  = 1024
          mftfit  = 2048
          call cffti(mftfit, wfftc)
          first  =  .false.
       end if
c
c 
c-----------------------------------------------------------------------
c      read inputs from command file with keywords
c-----------------------------------------------------------------------
c                        autobk.inp has already been opened as unit #1
 180   format(a)
 200   continue
          errskp = .false.  
          str    = ' '
          keywrd = ' '
          key    = ' '
          call getcom(jinit,string)
          call fixstr(string,str,ilen,words,wrdsor,maxwrd,nwords)
          if (ilen.lt.2) go to 200
          nline  = nline + 1
c
c  if line of minus signs is read, suspend reading of input file
c     until next data set
          if ((str.eq.'getcom_end').or.(str(2:5).eq.'----')) go to 500
          if (str.eq.'getcom_nofile') go to 1000
          if (str.eq.'getcom_error')  go to 1030
c 
c  interpret current words 
 300      continue
          if (nwords.le.0) go to 200
          keywrd = words(1)
          key    = keywrd(1:3)
          iwrds  = 2
c
c----read keywrd and get the right value
          if ( (key.eq.'tit').or.(key.eq.'com') ) then
             call triml(wrdsor(2))
             if (wrdsor(2).ne.' ') then
                i2 = max(1, istrln(wrdsor(2)))
                ix = index( string,wrdsor(2)(:i2) )
                commnt = string(ix:)
             end if
             go to 200
          elseif ( (keywrd.eq.'data').or.(keywrd.eq.'xmu') ) then
             i2 = max(1, istrln(wrdsor(2)))
             errskp = .true.
             if (nwords.ge.3) then
                i3 = max(1, istrln(wrdsor(3)))
                strdat = wrdsor(2)(:i2+2)//wrdsor(3)(:i3)
                call filrec(strdat, xmuf, skeyxm, nkeyxm)
             else
                xmuf   = wrdsor(2)
                nkeyxm = 0
                skeyxm = ' '
             end if
             dorun = .true.
          elseif( (keywrd(1:4).eq.'theo')
     $            .or.(keywrd(1:4).eq.'stan') ) then
             i2 = max(1, istrln(wrdsor(2)))
             errskp = .true.
             theory = .true.
             if (nwords.ge.3) then
                i3 = max(1, istrln(wrdsor(3)))
                strdat = wrdsor(2)(:i2+2)//wrdsor(3)(:i3)
                call filrec(strdat, theorf, skeyth, nkeyth)
             else
                theorf = wrdsor(2)
                nkeyth = 0
                skeyth = ' '
              end if
           elseif ((key.eq.'out').or.(keywrd.eq.'chi')) then
              chif   = words(2)
           elseif ((keywrd.eq.'form').or.(keywrd(1:5).eq.'forma')) then
              frminp = words(2)
              frmout = frminp
           elseif (keywrd.eq.'formin') then
              frminp = words(2)
           elseif (keywrd.eq.'formout')  then
              frmout = words(2)
c  --energy values
           elseif((key.eq.'ee').or.(key.eq.'e0')) then
              call str2dp(words(2),  ee, ierr)
              eefind = .false.
           elseif ((key.eq.'eef').or.(key.eq.'e0f')) then
              call str2dp(words(2),  ee, ierr)
              eevary = .false.
              eefind = .false.
           elseif (keywrd.eq.'fixe0') then
              call str2lg(words(2),  eevary, ier)
              eevary = .not.eevary
           elseif ((keywrd.eq.'thefix').or.(keywrd.eq.'fixthe')) then
              call str2lg(words(2),  thefix, ier)
           elseif (keywrd.eq.'fixamp') then
              call str2lg(words(2),  thefix, ier)
           elseif ((keywrd.eq.'predg1').or.(keywrd.eq.'pre1')) then
              call str2dp(words(2),  predg1, ierr)
           elseif ((keywrd.eq.'predg2').or.(keywrd.eq.'pre2')) then
              call str2dp(words(2),  predg2, ierr)
           elseif (keywrd.eq.'nterp') then
              call str2in(words(2),  nterp, ierr)
           elseif (keywrd.eq.'nnorm') then
              call str2in(words(2),  nnorm, ierr)
           elseif (keywrd.eq.'nor1') then
              call str2dp(words(2),  enor1, ierr)
           elseif (keywrd.eq.'nor2') then
              call str2dp(words(2),  enor2, ierr)
           elseif ((key.eq.'ste').or.(key.eq.'edg')) then
              call str2dp(words(2),  step, ierr)
              stfind = .false.
           elseif (keywrd(1:4).eq.'emin') then
              call str2dp(words(2),  emin, ierr)
           elseif (keywrd(1:4).eq.'emax') then
              call str2dp(words(2),  emax, ierr)
           elseif ((key.eq.'kmi').or.(key.eq.'qmi')) then
              call str2dp(words(2),  qmin, ierr)
              emin = qmin**2 / etok
           elseif ((key.eq.'kma').or.(key.eq.'qma')) then
              call str2dp(words(2),  qmax, ierr)
              emax = qmax**2 / etok
           elseif (keywrd(1:5).eq.'mucol') then
              call str2in(words(2),  imucol, ierr)
c
c-fourier transform
           elseif ((key(1:2).eq.'kw').or.(key(1:2).eq.'qw')
     $             .or.(key.eq.'w')) then
              call str2dp(words(2),  qweigh, ierr)
           elseif ((key.eq.'dk1').or.(key.eq.'dq1') ) then
              call str2dp(words(2),  windo1, ierr)
           elseif ((key.eq.'dk2').or.(key.eq.'dq2') ) then
              call str2dp(words(2),  windo2, ierr)
           elseif ((key.eq.'dk').or.(key.eq.'dq') ) then
              call str2dp(words(2),  windo1, ierr)
              windo2 = windo1
           elseif (key.eq.'han') then
              call str2dp(words(2),  windo1, ierr)
              iwindo = 1
              winstr   = 'hanning'
           elseif (key.eq.'win')  then
              if (words(2)(1:3).eq.'han') then
                 winstr   = 'hanning'
                 iwindo = 1
              elseif (words(2)(1:3).eq.'gau') then
                 winstr   = 'gaussian'
                 iwindo = 2
              elseif (words(2)(1:3).eq.'kai') then
                 winstr   = 'kaiser'
                 iwindo = 3
              elseif (words(2)(1:3).eq.'par') then
                 winstr   = 'parzen'
                 iwindo = 4
              elseif (words(2)(1:3).eq.'wel') then
                 winstr   = 'welch'
                 iwindo = 5
              end if
           elseif (keywrd(1:4).eq.'iwin')  then 
              call str2in(words(2),  iwindo, ierr)
           elseif ( (key.eq.'rma').or.(key.eq.'rbk') ) then
              call str2dp(words(2),  rbkg, ierr)
           elseif (key(1:2).eq.'r1') then
              call str2dp(words(2),  r1st, ierr)
c - -fourier transform, 
c - -fit, normalization flags              
           elseif (keywrd.eq.'nknots') then
              call str2in(words(2),  nsplin, ierr)
              gvknot = .true.
           elseif ((keywrd.eq.'norm').or.
     $             (keywrd(1:4).eq.'nor ')) then
              if (words(2)(1:3).eq.'fun')   funnrm = .true.
              if (words(2)(1:3).eq.'bkg')   funnrm = .true.
              if (words(2)(1:3).eq.'ste')   funnrm = .false.
              if (words(2)(1:3).eq.'edg')   funnrm = .false.
              if (words(2)(1:3).eq.'num')   funnrm = .false.
              iwrds = 3
c - -output flags
           elseif (keywrd.eq.'toler')  then 
              call str2dp(words(2),  usrtol, ierr)
           elseif ((keywrd.eq.'stiff').or.(keywrd.eq.'spstep')) then
              call str2dp(words(2),  spstep, ierr)
           elseif (keywrd.eq.'iprint')  then 
              call str2in(words(2),  iprint, ierr)
           elseif ((keywrd.eq.'preedge_out').or.
     $             (keywrd.eq.'preout')) then
              call str2lg(words(2),  preout, ier)
              nrmout = preout
           elseif ((keywrd.eq.'norm_out').or.
     $             (keywrd.eq.'nrmout')) then
              call str2lg(words(2),  nrmout, ier)
           elseif ((keywrd.eq.'eshift_out').or.
     $             (keywrd.eq.'eshout')) then 
              call str2lg(words(2),  eshout, ier)
           elseif ((keywrd.eq.'bkgout').or.(keywrd.eq.'bkgxmu')) then
              call str2lg(words(2),  bkgxmu, ier)
           elseif ((keywrd.eq.'bkgksp').or.(keywrd.eq.'bkgchi')) then
              call str2lg(words(2),  bkgchi, ier)
           elseif (keywrd.eq.'bkgrsp') then
              call str2lg(words(2),  bkgrsp, ier)
           elseif ((keywrd.eq.'theksp').or.(keywrd.eq.'thechi')) then
              call str2lg(words(2),  thechi, ier)
           elseif (keywrd.eq.'thersp') then
              call str2lg(words(2),  thersp, ier)
           elseif((keywrd.eq.'chirsp').or.(keywrd.eq.'datrsp')) then
              call str2lg(words(2),  chirsp, ier)
           elseif (key.eq.'all')  then
              call str2lg(words(2),  chirsp, ier)
              bkgxmu = chirsp
              bkgchi = chirsp
              thechi = chirsp
              thersp = chirsp
c comment char for ascii column data files
           elseif ((keywrd.eq.'comment_char').or.
     $             (keywrd.eq.'asccmt')) then
             asccmt  = words(2)(1:2)
c hardwire number of doc lines for ascii column data files
          elseif ((keywrd.eq.'doc_lines').or.
     $            (keywrd.eq.'mdocxx')) then
             call str2in(words(2), mdocxx, ierr )
c-- if the word wasn't recognized as a keyword, skip it and go on
           elseif (.not.errskp) then
              iw = max( 1, istrln(keywrd) )
              is = max( 1, istrln(string) )
              write(messg,'(3a)') 'warning: unknown keyword  < ',
     $             keywrd(1:iw),' > '
              iii   = max(1, istrln(messg))
              call echo( '   '//messg(1:iii))
              messg = '    " '//string(1:is)//' "'
              iii   = max(1, istrln(messg))
              call echo(messg(:iii))
              iwrds = 1
          end if
          if (nwords.gt.iwrds) then
             do 450 i = 1, nwords
                words(i) = words(i+iwrds)
 450         continue
             nwords = nwords - iwrds
             go to 300
          end if
          go to 200
c-----------------------------------------------------------------------
c  done reading the input file:
c  if we got to this line without setting domore to true, then
c  we read no input file name, so we'll return and stop the run
 500    continue
        domore = (str.ne.'getcom_end')  
c
        if (dorun) then
           string = 'autobk: '//xmuf
           ii = max(1, istrln(string))
           call echo('   '//string(1:ii)  )
           string = '        '// commnt
           ii = max(1, istrln(string))
           call echo('   '//string(1:ii)  )
c  output file names: find last '.', and 
c  save position 1 before '.'  in iodot
           if (chif.eq.' ') chif = xmuf
           call triml(chif)
           iodot  = max(1,istrln(chif)) 
           if (index(chif(1:iodot),'.').ne.0) then 
 623          continue
              iodot = iodot - 1
              if (index(chif(iodot+1:iodot+1),'.').eq.0) go to 623
           end if
           chif = chif(1:iodot)//'.chi'
        end if
c normal exit
        return
c-----------------------------------------------------------------------
c end subroutine autinp
 1000  continue
       call fstop(' autobk error: could not find autobk.inp')
 1010  continue
       call fstop(' autobk error: error opening autobk.inp')
 1020  continue
       call fstop(' autobk error: error opening autobk.log')
 1030  continue
       call fstop(' autobk: error reading autobk.inp')
       end
