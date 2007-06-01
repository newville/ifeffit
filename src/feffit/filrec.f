      subroutine filrec(string,filnam,skey,nkey)
c
c      takes a character string and reads from it a filename, and an
c  skey and/or nkey for a record. blanks, commas, or equal signs can
c  separate the inputs on the command line.
c
      character*128  temp, words(3)
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




