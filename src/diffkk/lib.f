      subroutine messag(messg)
c  write message to  standard ouput with (1x,a) format
      character*(*) messg
      write(*,'(1x,a)')   messg
      return
c end subroutine messag
      end

       subroutine getcom(prompt, iuin, commnd, files, iunit,
     $                   mfiles, nfiles)
c
c   get next command line from unit iuin, or from a command file. 
c   some rudimentary error checking is done here.
c
c   prompt     string to display when expecting input      (in)
c   iuin        default unit to read from                  (in)
c   files      array of command files to use               (in/out)
c   mfiles     max number of command files to use          (in/out)
c   commnd     next command line to execute                (out)
c
       integer mwords, ii, ipro, mfiles, nfiles
       parameter (mwords = 3) 
       character*(*)  prompt, commnd, files(mfiles) 
       character*80   line, words(mwords), errmsg, prom, stat*7
       integer        iunit(mfiles), istrln, nwords
       external       istrln
       data     stat  /'old'/
c---------------------------------------------------------------------
       call triml(prompt)
       prom   = prompt
       ipro   = istrln(prom)
       if (ipro.le.1) then
          prom = ' '
          ipro = 1
       end if
       iu0    = iuin
       if (iu0.le.0) iu0 = 5
 30    format(1x,a, '>',$)
 40    format(a)
 100   continue
c  read command from prompt (standard input)
c  or from current input command lines from an external file
       line   = ' ' 
       commnd = ' '
       if ((nfiles.lt.0).or.(nfiles.gt.mfiles))  nfiles = 0
       if (nfiles.eq.0) then
          if ((iu0.eq.5).and.(ipro.gt.1)) then
             write(*, 30) prom(1:ipro)
          end if
          read (      iu0   ,40, err = 600, end = 600) line
       else
          read(iunit(nfiles),40, err =1000, end = 500) line
       end if
c
c     check if command line is 'load filename'.
c     if so, open that file, and put it in the files stack
       call triml(line)
       call untab(line)
       nwords = mwords
       words(1) = ' '
       words(2) = ' '
       call bwords(line, nwords, words)
       if ((line.eq.' ').or.(nwords.le.0)) go to 100
       call smcase(words(1),'a')
       if ( (words(1)(1:5).eq.'load ').or.
     $      (words(1)(1:8).eq.'include ').or.
     $      (words(1)(1:6).eq.'input '))   then
          if (words(2).eq.' ') then
             call messag( ' ##>     no file name given. syntax is'//
     $                    '   include  filename ')
             go to 100
          end if
          nfiles = nfiles + 1
          if (nfiles.gt.mfiles) go to 2000
          call getfln(words(2),files(nfiles), errmsg)
          do 400 i = 1, nfiles - 1
             if (files(nfiles).eq.files(i)) go to 3000
 400      continue 
          iunit(nfiles) = 0 
          call openfl(iunit(nfiles), files(nfiles), stat, iexist, ierr)
          if (iexist.lt.0) go to 2600
          if (ierr.lt.0)   go to 2800
          go to 100
       elseif ((words(1)(1:1).eq.'*').or.(words(1)(1:1).eq.'#')) then
          commnd = ' '
          go to 100
       else
          commnd = line
       end if
       return
c
c  end-of-file for command line file: drop nfiles by 1,
c  return to get another command line
 500   continue 
       close(iunit(nfiles)) 
       nfiles = nfiles - 1
       if (nfiles.lt.0) nfiles = 0
       if (ipro.gt.1) go to 100
       commnd = 'getcom_eof'
       return
c
 600   continue 
       commnd = 'getcom_end'
       return
c
c     warning and error messages 
 1000  continue 
       call messag(' ##> error reading from "include"d file: '  )
       errmsg = files(nfiles)
       ii     = max(1, istrln(errmsg))
       call messag(' ##> '//errmsg(1:ii) )
       go to 5000
 2000  continue 
       call messag(' ##> error: too many nested "include"d files: '  )
       write(errmsg, '(1x,a,i3)') ' ##>current limit is ', mfiles
       ii     = max(1, istrln(errmsg))
       call messag(' ##> '//errmsg(1:ii) )
       go to 5000
 2600  continue 
       call messag(' ##> error: can not find "include"d file: '  )
       errmsg = files(nfiles)
       ii     = max(1, istrln(errmsg))
       call messag(' ##> '//errmsg(1:ii) )
       go to 5000
 2800  continue  
       call messag(' ##> error: can not open "include"d file: '  )
       errmsg = files(nfiles)
       ii     = max(1, istrln(errmsg))
       call messag(' ##> '//errmsg(1:ii) )
       go to 5000
 3000  continue 
       call messag(' ##> error: recursive "include" of file:')
       errmsg = files(nfiles)
       ii     = max(1, istrln(errmsg))
       call messag(' ##> '//errmsg(1:ii) )
       go to 5000
c
 5000  continue
          commnd = 'getcom_error'
          return
c     
c end  subroutine getcom
       end
       subroutine fixstr(string,str,ilen,words,wrdsor,mwords,nwords)
c  simple preparation of string for reading of keywords
       integer       ilen, mwords, nwords, i, lenp1
       integer       iexcla, iperct, ihash, ieolc, istrln
       character*(*) string, str, words(mwords), wrdsor(mwords)
c
c  fix-up string: untab, left-justify, make a lower-case version
       nwords = 0
       call untab(string)
       str   = string
       call triml(str)
       call smcase( str, 'case')
c  remove comments from str:
c   '!', '#', and '%' are end of line comments
c   '*' is a complete comment line if in col 1
       lenp1  = len(str) + 1
       iexcla = index(str,'!')
       if (iexcla.eq.0)  iexcla = lenp1
       iperct = index(str,'%')
       if (iperct.eq.0)  iperct = lenp1
       ihash  = index(str,'#')
       if (ihash.eq.0)  ihash = lenp1
       ieolc  = min(iperct,iexcla,ihash) - 1
       if ((ieolc.lt.1).or.(str(1:1).eq.'*')) ieolc = 1
       str    = str(1:ieolc)
       ilen   = max(1, istrln(str))
       if (ilen.le.2)  return
c  break string into words (up to mwords)
c  words is in lower case,   wrdsor is in original case
       do 120 i = 1, mwords
          words(i)   =  ' '
          wrdsor(i) =  ' '
 120   continue
       nwords = mwords
       call bwords(str   , nwords, words)
       call bwords(string, nwords, wrdsor)
c end  subroutine fixstr
       return
       end
       subroutine askstr(ask, str)
c
c      prompt for and return a characer string.
c      see also the routines askint, and askval.
c  inputs: 
c    ask   character string for prompt
c    str   default string to show in prompt
c  outputs:
c    str   string read in 
c  copyright 1993 university of washington     matt newville         
      character*(*) ask , str
      character*80  query , answer
      integer   i , j, k, istrln
      external istrln
      query = ask
      call triml(query)
      call triml(str)
      i    = max(1, istrln(query)    ) 
      j    = max(1, istrln(str)      )
      k    = max(1, min(80, len(str))) 
 30   format (2x,a,' [', a, ']  >',$)
      write(*, 30  ) query(1:i), str(1:j)
      read (*, '(a)', err= 50) answer
      call triml(answer)
      if (istrln(answer).ge.1) str = answer(1:k)
 50   continue
      return
c end subroutine askstr
      end
      subroutine askdp(ask, val)
c  prompt for and return a double precision number
c  inputs:
c    ask   character string for prompt 
c    val   default dp number to show in prompt
c  outputs:
c    val   dp number read in 
c  copyright 1993 university of washington     matt newville         
      character*(*) ask, answer*30 , query*80
      integer       i, istrln
      double precision   val, tmp
      external      istrln
      query = ask
      i    =  max(1, istrln(query) )
 30   format( 2x,a,' [', g16.8, ']  >',$)
      write(*, 30) query(1:i), val
      read (*, '(a)', err = 50) answer
      if ( answer.ne.' ') then
         call str2dp(answer, tmp, ierr)
         if (ierr.eq. 0 )  val = tmp
      end if
 50   continue
      return
c end subroutine askdp
      end
       subroutine askint(ask, int)
c  prompt for and return an integer.
c  inputs:
c    ask   character string for prompt 
c    int   default integer to show in prompt
c  outputs:
c    int   integer read in 
c  copyright 1993 university of washington     matt newville         
       character*(*) ask, answer*30, query*80
       integer       i, istrln, int, itmp
       external      istrln
       query = ask
       call triml(query)
       i    =  max(1, istrln(query) )
 30    format( 2x,a,' [', i4, ']  >',$)
       write(*, 30) query(1:i), int
       read (*, '(a)', err = 50) answer
       call triml(answer) 
       if (istrln(answer).ge.1) then
          call str2in(answer, itmp, ierr)
          if (ierr.eq. 0 )  int = itmp
       end if
 50    continue
       return
c end subroutine askint
       end
      character*2 function atsym (iz)
      character*2 sym(103)
      common /atsyms/ sym
      save
      atsym = 'xx'
      if ((iz.le.103).and.(iz.gt.0)) atsym = sym(iz)
      call upper(atsym(1:1))
      return
      end
c
       integer function iatsym (symin)
       character*2 sym(103), symin
       common /atsyms/ sym
       save
       call smcase(symin,sym(1))
       do 10 iatsym = 1, 103
          if (symin.eq.sym(iatsym)) return
 10    continue 
       iatsym = 0
       return
       end
c
       block data prtabl
       character*2 sym(103)
       common /atsyms/ sym
       data sym / 'h' ,'he','li','be','b' ,'c' ,'n' ,'o' ,'f' ,'ne',
     $  'na','mg','al','si','p' ,'s' ,'cl','ar','k' ,'ca','sc','ti',
     $  'v' ,'cr','mn','fe','co','ni','cu','zn','ga','ge','as','se',
     $  'br','kr','rb','sr','y' ,'zr','nb','mo','tc','ru','rh','pd',
     $  'ag','cd','in','sn','sb','te','i' ,'xe','cs','ba','la','ce',
     $  'pr','nd','pm','sm','eu','gd','tb','dy','ho','er','tm','yb',
     $  'lu','hf','ta','w' ,'te','os','ir','pt','au','hg','tl','pb',
     $  'bi','po','at','rn','fr','ra','ac','th','pa','u' ,'np','pu',
     $  'am','cm','bk','cf','es','fm','md','no','lw'/
       end

