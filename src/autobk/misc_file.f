      subroutine filrec(string,filnam,skey,nkey)
c
c      takes a character string and reads from it a filename, and an
c  skey and/or nkey for a record. blanks, commas, or equal signs can
c  separate the inputs on the command line.
c
      character*100  temp, words(3)
      character*(*)  string ,   filnam , skey
c
      nkey = 0
      skey = ' '
      nwords = 3
      call bwords(string,nwords,words)
c---- first word is filename
      filnam = words(1)
      nwords = nwords - 1
c---- second word is nkey or skey
      temp   = words(2)
c---- determine if second//third word is nkey/skey
c     skeys are exactly 5 characters long,
c     nkeys are never more than 3 characters long
 50   continue
      nwords = nwords - 1
      call triml(temp)
      ilen = istrln(temp)
      if(ilen.eq.5) then
         skey = temp
         call upper(skey)
      elseif(ilen.eq.4) then
         call fstop('error reading skey or nkey from '//temp)
      else
         call str2in(temp, nkey, ierr)
      end if
c---- the third word, if it exists
      if (nwords.eq.1) then
         temp = words(3)
         call triml(temp)
         go to 50
      end if
 1000 return
c     end subroutine filrec
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
      integer function nxtunt(iunit)
c  return next available unit number, greater than or equal to iunit.
c  will not return unit number less than 1, or equal to 5 or 6.
      integer iunit
      logical open

      nxtunt = max(1, iunit)
 10   continue
      inquire (unit=nxtunt, opened=open)
      if (open) then
          nxtunt = nxtunt + 1
          if ((nxtunt.eq.5).or.(nxtunt.eq.6)) nxtunt = 7
          goto 10
      endif
      return
c  end integer function nxtunt
      end
      logical function iscomm(str)
c true if str is a comment line or blank line, false otherwise
      character*(*) str
      iscomm = ((str.eq.' ') .or. (index('*%#',str(1:1)).ne.0))
      return
      end
