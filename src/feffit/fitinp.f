       subroutine fitinp
c
c  read inputs for feffit from feffit.inp
c  copyright 1993 university of washington          matt newville
c
c  read inputs from command file with keywords and a lot of elseifs.
c
       include 'fitcom.h'
c
       integer maxwrd, mfil, i, iinp, iex, ier, nwords, iwrds
       integer ilen, istrln, j, ierr, i2, iv, i3, jinit, ie
       integer nfil, jl, iuser, idpath, inpath, iffx, iunky
       parameter(maxwrd = 30, mfil = 10)
       character*2048 str, string, strdum, stat*10
       character*128 words(maxwrd), wrdsor(maxwrd) 
       character*32  wins(8)
       character     keywrd*128, key*3, prompt*20
       logical       errskp, path0, flag
       integer       itemp, itfeff, ifeff, ititle, idata, iparam
       integer       ilcl, jlcl,icom(mfil), ix
       external      istrln
c
c      initialization
       itemp  = 0
       itfeff = 0
       ititle = 0
       idata  = 1
       ilcl   = 0
       ix      = 0
       wins(1) =  'hanning'
       wins(2) =  'fhanning'
       wins(3) =  'gaussian window'
       wins(4) =  'kaiser-bessel'
       wins(5) =  'parzen'
       wins(6) =  'welch'
       wins(7) =  'sine'

       prompt = 'f'
       do 10 i = 1, mfil
          icom(i)   = 0
 10    continue
       nfil   = 0
       iunky  = 0
c   input file: if file is found, open it for reading,
       iinp = 1
       ier  = 0
       iex  = 0
       stat = 'old'
       call  get_inpfile('feffit.inp',inpfil,ier)
       call lower(inpfil)
       if (macflg) then
cc#mac
ccc use LS Fortran's '*' syntax and dialog boxes (thanks boyan!)
c          open(unit=iinp,file=*,status='old',iostat=ier)
c          if (ier.ne.0) then
c             call AlertBox('File selection was canceled!')
c             call finmsg(2001,string,' ',0)
c          end if
c          call f_setvolume(jvrefnum(iinp))
c          call f_creator('ttxt')
ccc   this resets fname to the name of the opened file.
ccc   useful for computing output file names, etc.
c           inquire(unit=iinp,name=inpfil,iostat=ier)
c           if (ier.ne.0) inpfil='feffit.inp'
cc#mac
       else
cc          call openfl(iinp, inpfil, stat, iex, ier)
cc          if (iex.lt.0) call finmsg(1001,inpfil,' ',0)
cc          if (ier.ne.0) call finmsg(1002,inpfil,' ',0)
       end if
c---------------------------------------------------------------------
c     read in next line
       jinit  = -1
       string = inpfil
 100   continue
          errskp = .false.
          path0  = .false.
          iparam = jpnull
          strdum = ' '
          keywrd = ' '
          key    = ' '
          call getcom(jinit,string)
          call fixstr(string,str,ilen,words,wrdsor,maxwrd,nwords)
          if (ilen.lt.2) go to 100
          if (str.eq.'getcom_end')  go to 4000
          if (str.eq.'getcom_error') call finmsg(1003,inpfil,' ',0)
          if (str.eq.'getcom_nofile')
     $         call finmsg(1001,inpfil,' ',0)
 150      continue
          keywrd  = words(1)
          key     = keywrd(1:3)
          iwrds   = 2
c     use keyword to get the right value
c--   data set index : ' next data set ' on its own line
          if ( (keywrd.eq.'next').and.(words(2).eq.'data')) then
             iwrds  = maxwrd + 1
c   first write out title lines from previous data set
             messg = '   titles:'
             ilen = istrln(messg)
             write(strdum,'(a,i4)') ' --> data set #',idata
             call append(messg,strdum,ilen)
             call echo( messg(1:ilen))
             do 225 j = 1, mtitle
                if (titles(j,idata) .ne. ' ' )
     $               call echo('      '//titles(j,idata)(1:65))
 225         continue
c    now increment idata, reset ititle, check that idata isn't too big
             idata  = idata  + 1
             ititle = 0
             if (idata.gt.mdata) call finmsg(2220,string,' ',mdata)
c
c     keywords for input/ouput
c--   title  ( on its own line)
          elseif (keywrd.eq.'title') then
             iwrds  = maxwrd + 1
             ititle = ititle + 1
             if ( (ititle.le.mtitle).and.(wrdsor(2).ne.' ')) then
                call strclp(string, wrdsor(1), wrdsor(2),
     $               titles(ititle, idata))
             end if
c--   end reading file : ignore everything in input file past this line
          elseif (( keywrd.eq.'end' ).or.( keywrd.eq.'quit')) then
             go to 4000
c--   file formats for inputs and outputs: defaults are uwxafs,
c     but can be changed to ascii column files with  formin = 'asc'
          elseif ((keywrd.eq.'form').or.(keywrd.eq.'format')) then
             frmout = words(2)
             frminp = frmout
          elseif (keywrd(1:7).eq.'formin') then
             frminp = words(2)
          elseif (keywrd(1:8).eq.'formout')  then
             frmout = words(2)
c comment char for ascii column data files
          elseif ((keywrd.eq.'comment').or.(keywrd.eq.'asccmt')) then
             asccmt  = words(2)
c hardwire number of doc lines for ascii column data files
          elseif (keywrd.eq.'mdocxx') then
             call str2in(words(2), mdocxx, ierr )
c--   input data file        note: for uwexafs files, strdum contains
c     the file name and record specifier (nkey or skey).
          elseif (keywrd.eq.'data')  then
             datain(idata) = .true.
             errskp        = .true.
             i2 = max(1, istrln(wrdsor(2)))
             if (nwords.ge.3) then
                i3 = max(5, istrln(wrdsor(3)))
                strdum = wrdsor(2)(:i2+2)//wrdsor(3)(:i3)
                call filrec(strdum, chifil(idata), skey(idata),
     $                      nkey(idata))
             else
                chifil(idata) = wrdsor(2)
                skey(idata)   = ' '
                nkey(idata)   = 0
             end if
c--   bkg(k) data file     note: for uwexafs files, strdum contains
c     the file name and record specifier (nkey or skey).
          elseif (keywrd(1:6).eq.'bkgfil')  then
             bkgdat(idata) = .true.
             errskp        = .true.
             i2 = max(1, istrln(wrdsor(2)))
             if (nwords.ge.3) then
                i3 = max(5, istrln(wrdsor(3)))
                strdum = wrdsor(2)(:i2+2)//wrdsor(3)(:i3)
                call filrec(strdum, bkgfil(idata), skeyb(idata),
     $                      nkeyb(idata))
             else
                bkgfil(idata) = wrdsor(2)
                skeyb(idata)   = ' '
                nkeyb(idata)   = 0
             end if
c--   output file
          elseif  (keywrd.eq.'out') then
             outfil(idata)   = wrdsor(2)
c--   print level to feffit.run file
          elseif (keywrd.eq.'iprint') then
             call str2in(words(2), iprint, ierr )
c--   flag for doing tranquada correction
          elseif (keywrd(1:5).eq.'tranq') then
             call str2dp(words(2), tranq, ierr )
c--   flag for not doing fit and writing output
          elseif (keywrd.eq.'norun') then
             call str2lg(words(2), noout, ierr )
             nofit = noout
c--   flag for not writing output
          elseif (keywrd.eq.'noout') then
             call str2lg(words(2), noout, ierr )
c--   flag for not doing fit
          elseif (keywrd.eq.'nofit') then
             call str2lg(words(2), nofit, ierr )
c--  flag for writing feffit.prm file
          elseif (keywrd.eq.'prmout') then
             call str2lg(words(2), prmout, ierr )
c--  flag for writing phase corrected FT
          elseif (keywrd.eq.'pcout') then
             call str2lg(words(2), pcout, ierr )
cc--  flag for using phase corrected FT in fit
c          elseif (keywrd.eq.'pcfit') then
c             call str2lg(words(2), pcfit, ierr )
c--   flag for fitting background spline to low-r components
          elseif (keywrd.eq.'bkg') then
             call str2lg(words(2), bkgfit(idata), ierr )
c--   flag for writing background spline to output data file
          elseif (keywrd.eq.'bkgout') then
             call str2lg(words(2), bkgout, ierr )
c--   flag for writing out data for all paths
          elseif ((keywrd.eq.'all').or.(keywrd.eq.'allout')) then
             call str2lg(words(2), allout, ierr )
c--   flag for writing out data in k space
          elseif ((keywrd.eq.'kfull').or.(keywrd.eq.'fullk')) then
             call str2lg(words(2), kspcmp, ierr )
c--   flag for writing out data in k space
          elseif (keywrd.eq.'kspout') then
             call str2lg(words(2), kspout, ierr )
c--   flag for writing out data in r space
          elseif (keywrd.eq.'rspout') then
             call str2lg(words(2), rspout, ierr )
c--   flag for writing out data in q space (backtransform)
          elseif ((keywrd.eq.'qspout').or.(keywrd.eq.'envout')) then
             call str2lg(words(2), qspout, ierr )
c--   flags for which space to fit in (default is ifft=1 for r-space)
          elseif (keywrd.eq.'kspfit') then
             call str2lg(words(2), flag, ierr)
             if (flag)  ifft(idata) = 0
          elseif (keywrd.eq.'rspfit') then
             call str2lg(words(2), flag, ierr )
             if (flag)  ifft(idata) = 1
          elseif (keywrd.eq.'qspfit') then
             call str2lg(words(2), flag, ierr)
             if (flag)  ifft(idata) = 2
          elseif (keywrd.eq.'fit_space') then
             if (words(2)(1:1).eq.'k') ifft(idata) = 0
             if (words(2)(1:1).eq.'r') ifft(idata) = 1
             if (words(2)(1:1).eq.'q') ifft(idata) = 2
c--   flag for not using degeneracies from feff
          elseif (keywrd.eq.'degen') then
             call str2lg(words(2), degflg, ierr )
          elseif (keywrd.eq.'nodegen') then
             call str2lg(words(2), degflg, ierr )
             degflg = .false.
c--   relative weights for different data sets
          elseif (keywrd.eq.'weight') then
             call str2dp(words(2), sigwgt(idata), ierr )
c--   error bars  measurement uncertainty (default is to specify
c          in k-space, but r-space is also allowed.)
          elseif ( (keywrd.eq.'sigdat').or.(key.eq.'eps')
     $      .or.(keywrd.eq.'sigk').or.(keywrd.eq.'epsk')) then
             call str2dp(words(2), sigdtk(idata), ierr )
          elseif ( (keywrd.eq.'sigr').or.(keywrd.eq.'epsr')) then
             call str2dp(words(2), sigdtr(idata), ierr )
c--   maximum correlation to report
          elseif (keywrd.eq.'cormin') then
             call str2dp(words(2), cormin, ierr )
          elseif (keywrd.eq.'rwght1') then
             call str2dp(words(2), rwght1, ierr )
          elseif (keywrd.eq.'rwght2') then
             call str2dp(words(2), rwght2, ierr )
c--  hack for playing with user tolerance
       elseif  (keywrd.eq.'toler') then
          call str2dp(words(2), usrtol, ierr )
c--  number of iterations to make when evaluating error bars
       elseif  (keywrd.eq.'nerstp') then
         call str2in(words(2), nerstp, ierr )
c     keywords for feff.dats
c--   maximum r to write for r-space data
          elseif (keywrd.eq.'rlast') then
             call str2dp(words(2), rlast, ierr )
c     keywords for fft stuff
c--   number of points in fft for fit         ( found from qmin, qmax)
c          elseif (keywrd.eq.'max_fft_fit') then
c             call str2in(words(2), mftfit, ierr )
cc--   number of points in fft for writing out data ( 2048)
c          elseif (keywrd.eq.'max_fft_out') then
c             call str2in(words(2), mftwrt, ierr )
c--   minimum r for fit range
          elseif (keywrd.eq.'rmin') then
             call str2dp(words(2), rmin(idata), ierr )
c--   maximum r for fit range
          elseif (keywrd.eq.'rmax') then
             call str2dp(words(2), rmax(idata), ierr )
c--   minimum k for fit range / fourier transform
          elseif ((keywrd.eq.'kmin').or.(keywrd.eq.'qmin')) then
             call str2dp(words(2), qmin(idata), ierr )
c--   maximum k for fit range / fourier transform
          elseif ((keywrd.eq.'kmax').or.(keywrd.eq.'qmax')) then
             call str2dp(words(2), qmax(idata), ierr )
c--   k weight for fourier transform
          elseif ((key(1:2).eq.'kw').or.(key(1:2).eq.'qw')
     $            .or.(key.eq.'w  ')) then
             call str2dp(words(2), qweigh(idata), ierr )
c--   window sill fourier transform window parameter(s)
          elseif (keywrd(1:4).eq.'win') then
             sqwin(idata) = words(2)
          elseif (keywrd(1:4).eq.'iwin') then
             call str2in(words(2), ix, ierr )
             sqwin(idata) = wins(ix+1)
             srwin(idata) = sqwin(idata)
c--   window sill fourier transform window parameter(s)
          elseif ((keywrd.eq.'iqwin').or.(keywrd.eq.'ikwin')) then
             call str2in(words(2), ix, ierr )
             sqwin(idata) = wins(ix+1)
c--   window sill fourier transform window parameter(s)
          elseif (keywrd.eq.'irwin') then
             call str2in(words(2), ix, ierr )
             srwin(idata) = wins(ix+1)
cc--   gaussian fourier window
c          elseif (keywrd.eq.'gauss')  then
c             call str2dp(words(2), qwin1(idata), ierr )
c             qwin2(idata) = qwin1(idata)
c             iqwin(idata) = 2
c             irwin(idata) = 2
cc--   hanning fraction window
c          elseif ((keywrd.eq.'fhan').or.(keywrd.eq.'hann')) then
c             call str2dp(words(2), qwin1(idata), ierr )
c             qwin2(idata) = qwin1(idata)
c             iqwin(idata) = 1
c             irwin(idata) = 1
c-- k-space window parameters
          elseif ((keywrd.eq.'dk2').or.(keywrd.eq.'dq2')) then
             call str2dp(words(2), qwin2(idata), ierr )
          elseif ((keywrd.eq.'dk1').or.(keywrd.eq.'dq1')) then
             call str2dp(words(2), qwin1(idata), ierr )
          elseif ((key.eq.'dk').or.(key.eq.'dq')) then
             call str2dp(words(2), qwin1(idata), ierr )
             qwin2(idata) = qwin1(idata)
c-- r-space window parameters
          elseif (key.eq.'dr2') then
             call str2dp(words(2), rwin2(idata), ierr )
          elseif (key.eq.'dr1') then
             call str2dp(words(2), rwin1(idata), ierr )
cc          elseif (key.eq.'dr') then
cc             call str2dp(words(2), rwin1(idata), ierr )
cc             rwin2(idata) = rwin1(idata)
c-- optical transform phase factors
c          elseif (keywrd.eq.'rpha') then
c             call str2dp(words(2), rphas(idata), ierr )
c          elseif (keywrd.eq.'qpha') then
c             call str2dp(words(2), qphas(idata), ierr )
c
c     keywords for values: "guess"es and "set"s
c     for variables and functions
c--   variable: definition and initial guess
c--   here we : - increase the number of variables
c--   - set the initial guess of the variable
c--   - find the right value to associate with the variable
c--   - set icdval(1,iv) = -1 (to mark value as a variable)
          elseif (keywrd.eq.'guess')   then
             iwrds  = maxwrd + 1
             numvar = numvar + 1
             nvuser = numvar
c     find if this value was already defined,
c     or find the next avialable slot
c*mn -- make a function to return iv :
c    iv = nofstr(words(2),vnames,maxval)
c    that returns iv , or 0 if unfound
cc             iv  = nofstr(words(2), vnames, maxval)
cc             if (iv.eq.0) call finmsg(2100,string,' ',maxval)

             do 300  iv = 1, maxval
                if  (vnames(iv).eq.' ')       go to 310
                if  (vnames(iv).eq.words(2))  go to 320
 300         continue
             call finmsg(2100,string,' ',maxval)
c     new variable  : store name of value, increment # of variables
 310         continue
             nmathx     = max(iv, nmathx)
             vnames(iv) = words(2)
             if (numvar.gt.mvarys) call finmsg(2130,string,' ',mvarys)
c     previously defined value :
c     make sure it's set as a variable, and get initial value
 320         continue
             if (icdval(1, iv).ge.1) then
                call echo( '  -- feffit warning:  '//
     $               'confusion about variables ')
                messg =   vnames(iv)
                ilen = max(1, istrln(messg))
                call echo( '       '//messg(1:ilen))
                call echo( '        was "guessed" after '//
     $               'being "set".  results may be unstable.')
             elseif (icdval(1, iv).lt.0) then
                call echo( '  -- feffit warning:  '//
     $               'this variable was "guessed" twice:')
                messg =   vnames(iv)
                ilen = max(1, istrln(messg))
                call echo( '       '//messg(1:ilen))
             endif
             icdval(1,iv)   = -1
             call str2dp(words(3), xguess(numvar), ierr )
             values(iv)     = xguess(numvar)
c
c--   functions: definition and integer array
c--   here we : - check if value has already been defined.
c--   - find the right value to associate with the variable
c--   - encode the icdval integer array
          elseif (keywrd.eq.'set') then
             iwrds  = maxwrd + 1
             do 550  iv = 1, maxval
                if ( (vnames(iv).eq.words(2)).or.
     $               (vnames(iv).eq.' '     )      ) go to 560
 550         continue
             call finmsg(2200,string,' ',maxval)
 560         continue
             nmathx     = max(iv, nmathx)
             vnames(iv) = words(2)
c     check that this user-defined function was not previously
c     assigned as a local user-defined function. at this point,
c     just stop if it was.
             if (icdval(1,iv).gt.ixlocl)
     $            call finmsg(2110,string,vnames(iv),iv)
             call strclp(str,words(2),words(3),strdum)
             if (icdval(1, iv).lt.0) then
                call echo( '  -- feffit warning:  '//
     $               'confusion about variables ')
                messg =   vnames(iv)
                ilen = max(1, istrln(messg))
                call echo( '       '//messg(1:ilen))
                call echo( '        was "set" after '//
     $               'being "guessed".  results may be unstable.')
                numvar = numvar - 1
             elseif (icdval(1, iv).gt.0) then
                call echo( '  -- feffit warning:  '//
     $               'this variable was "set" twice:')
                messg =   vnames(iv)
                ilen = max(1, istrln(messg))
                call echo( '       '//messg(1:ilen))
             endif
             ierr = istrln(str)
c             print*, 'UBER: fitinp: ilen = ', ierr, ' :: ', str(1:ierr)
c             ierr = istrln(strdum)
c             print*, 'UBER: fitinp: ilen = ', ierr, ' :: ',
c     $            strdum(1:ierr)
             ierr   = 0
             call encod(strdum, vnames, maxval, consts, mconst,
     $            icdval(1,iv), micode, ierr)
             if (ierr.gt.0) call finmsg(2500,string,strdum,0)
c--   local functions: definition and integer array
c--   here we : - check if value has already been defined.
c--   - find the right value to associate with the variable
c--   - encode the icdval integer array
          elseif (keywrd.eq.'local') then
             iwrds  = maxwrd + 1
             iv     = 0
 750         continue
             iv = iv + 1
             if (iv.gt.maxval) call finmsg(2200,string,' ',maxval)
             if ( (vnames(iv).ne.words(2)).and.
     $            (vnames(iv).ne.' ')) go to 750
ccc             do 750  iv = 1, maxval
ccc                if ( (vnames(iv).eq.words(2)).or.
ccc     $               (vnames(iv).eq.' '     )      ) go to 760
ccc 750         continue
ccc             call finmsg(2200,string,' ',maxval)
ccc 760         continue
             nmathx     = max(iv, nmathx)
             vnames(iv) = words(2)
             jlcl = icdval(1,iv) - ixlocl
             if (jlcl.le.0) then
                ilcl  = ilcl + 1
                if (ilcl.gt.mlocal) call finmsg(2105,string,' ',mlocal)
                jlcl  = ilcl
             else
                do 780 jl  = 1, mlocal
                   if ((jlcl.eq.jl)) go to 790
 780            continue
                call finmsg(2105,string,' ',mlocal)
 790            continue
                jlcl = jl
             endif
c
c     check that this local user-defined function was not previously
c     assigned as a global user-defined function. at this point,
c     just stop if it was.
             if ((icdval(1,iv).ne.0).and.(icdval(1,iv).lt.ixlocl))
     $            call finmsg(2110,string,vnames(iv),iv)
             call strclp(str,words(2),words(3),strdum)
             ierr   = 0
             call encod(strdum, vnames, maxval, consts, mconst,
     $            icdloc(1,jlcl,idata), micode, ierr)
             icdval(1,iv) = ixlocl + jlcl
             if (ierr.gt.0) call finmsg(2120,string,strdum,0)
c
c     path parameters
c--   feff file name
          elseif ((keywrd.eq.'path').or.(keywrd.eq.'feff'))  then
             iparam = jppath
c--   user identification label
          elseif (key.eq.'id ')       then
             iparam = jplabl
c--   constant amplitude factor
          elseif ( (keywrd.eq.'s02').or.(keywrd.eq.'amp').or.
     $            (keywrd.eq.'so2')) then
             iparam = jps02
c--   energy shift : real energy correction
          elseif ( (key.eq.'esh').or.(key.eq.'e0 ').or.
     $            (key.eq.'ee ').or.(key.eq.'e0s'))  then
             iparam = jpe0
c--   energy shift : imaginary energy correction
          elseif (key.eq.'ei ') then
             iparam = jpei
c--   energy shift : imaginary energy correction
          elseif ((keywrd.eq.'dphase').or.(keywrd.eq.'phase')) then
             dphflg = .true.
             iparam = jpdpha
c--   delta r , the first cumulant
          elseif ( (keywrd.eq.'dr').or.(keywrd.eq.'deltar').or.
     $             (keywrd.eq.'delr'))   then
             iparam = jpdelr
c--   sigma^2, the debye waller factor, the second cumulant
          elseif ((keywrd(1:4).eq.'sigm').or.(keywrd.eq.'ss2')) then
             iparam = jpsig2
c--   the third cumulant
          elseif ( (keywrd.eq.'3rd').or.(keywrd.eq.'third').or.
     $            (keywrd.eq.'cubic'))  then
             iparam = jp3rd
c--   the fourth cumulant
          elseif ( (keywrd.eq.'4th').or.(keywrd.eq.'fourth').or.
     $            (keywrd.eq.'quartic'))  then
             iparam = jp4th
c--   didn't find anything! a null word or something.
          elseif ((.not.errskp).and.(iparam.eq.jpnull) ) then
             call finmsg(2330,string,keywrd,0)
             iwrds  = maxwrd + 1
             iunky  = iunky + 1
             if (iunky.ge.5) call finmsg(2300,string,' ',0)
          end if
c  keywords all checked now.
c
c  now, if keyword is a path parameter, interpret it.
c  there are a lot of path indices here, and they get confusing. 
c  here's a menu:
c     iuser   "user path index"      what the user wrote in feffit.inp
c     inpath  "internal path index"  which set of path params to use           
c     ifeff   "feff path index"      which feff file to use
c     idpath  "data path index"      which internal path is this for this
c                                    data set, when summing over paths
c  idpath is the key, and gives the rest using pointers in common blocks:
c     inpath = jdtpth(idpath,idata) 
c     iuser  = jdtusr(idpath,idata)
c     ifeff  = jpthff(inpath)
c
c--   iuser set here, and character string extracted
          if ( (iparam.ne.jpnull).and.(nwords.gt.2)) then
             iwrds  = maxwrd + 1
             call str2in(words(2), iuser, ierr )
             if ((iuser.lt.0).or.(iuser.gt.10000))
     $            call finmsg(2200,str,' ',0)
             path0  = (iuser.eq.0)
             call strclp(string,wrdsor(2),wrdsor(3),strdum)
c     assign a data path to the user path:
c--   idpath set here so that jdtusr(idpath,idata) = iuser
             idpath = 0
             if (.not.path0) then
                do 900 idpath = 1, mdpths
                   if (jdtusr(idpath,idata).eq.iuser)   go to 940
                   if (jdtusr(idpath,idata).eq.0) then
                      jdtusr(idpath,idata) = iuser
                      go to 940
                   end if
 900            continue
                call finmsg(2140,string,chifil(idata),mdpths)
 940            continue
             end if
c
c   assign an internal path to the data path:
c-- inpath set here           so that jdtpth(idpath,idata) = inpath
             do 1340 inpath = 1, mpaths
                if (jdtpth(idpath,idata).eq.inpath)  go to 1400
                if (jdtpth(idpath,idata).eq.0)       go to 1380
 1340        continue
             call finmsg(2150,string,' ',mpaths)
 1380        continue
             itemp  = itemp + 1
             inpath = itemp
             jdtpth(idpath,idata) = inpath
 1400        continue
c
c   assign a feff path to the internal path:
c-- ifeff set here          so that jpthff(inpath) = ifeff
             if ( (iparam.eq.jppath).and.(.not.path0)) then
                iffx = 0
                if (nwords.ge.4) call str2in(wrdsor(4),iffx,ie)
                do 1640 ifeff = 1, mfffil
                   if ( (feffil(ifeff) .eq. wrdsor(3)).and.
     $                  (iffrec(ifeff) .eq. iffx)) go to 1700
                   if (  feffil(ifeff) .eq. ' '  ) go to 1680
 1640           continue
                call finmsg(2170,string,feffil(ifeff),mfffil)
 1680           continue
                itfeff         = itfeff  + 1
                ifeff          = itfeff
                feffil(ifeff)  = wrdsor(3)
                iffrec(ifeff)  = iffx
cc                print*, ' INP path !  ', ifeff, feffil(ifeff)

 1700           continue
                if (jpthff(inpath).ne.0) then
                   write(messg,'(2a,i5)')' -- feffit warning: ',
     $                  'overwriting the feff file for path ', iuser
                   ilen = max(1, istrln(messg))
                   call echo( '       '//messg(1:ilen))
                end if
cc                print*, ' itfeff = ', itfeff, ifeff, inpath
                jpthff(inpath) = ifeff
             elseif ((iparam.eq.jppath).and.path0 ) then
                jpthff(inpath) = -1
                call echo( '  -- feffit warning: giving a '//
     $               'feff file for path 0 has no meaning.')
             elseif((iparam.eq.jplabl).and.(.not.path0)) then
                pthlab(inpath) = strdum
             else
                if (icdpar(1,iparam,inpath).ne.0) then
                   write(messg,'(4a,i5)')' -- feffit warning: ',
     $              'redefining ', parnam(iparam),' of path ',iuser
                   ilen = max(1, istrln(messg))
                   call echo( '       '//messg(1:ilen))
                end if
                call smcase(strdum, 'case')
                ierr      = 0
                call encod(strdum, vnames, maxval, consts, mconst,
     $               icdpar(1,iparam,inpath), micode, ierr)
                if (ierr.gt.0) call finmsg(2120,string,strdum,0)
             end if
          end if
c     decide whether or not to read more inputs from the current string
          if (nwords.gt.iwrds) then
             do 3600 i = 1, nwords
                words(i)  = words(i+iwrds)
                wrdsor(i) = wrdsor(i+iwrds)
 3600        continue
             nwords = nwords - iwrds
             go to 150
          end if
          go to 100
 4000  continue
c  end of reading input file
c  close input file, and write out titles for last data set
       if (iinp.ne.5) close(iinp)
       ndata = idata
cc       print*, 'FITINP: ndata = ', ndata
       messg = '   titles:'
       ilen = istrln(messg)
       if (ndata.gt.1) then
          write(strdum,'(a,i4)') ' --> data set #',idata
          call append(messg,strdum,ilen)
       end if
       call echo( messg(1:ilen))
       do 5800 j = 1, mtitle
          if (titles(j,idata) .ne. ' ' )
     $         call echo('      '//titles(j,idata)(1:65))
 5800  continue
       return
c end subroutine fitinp
       end
