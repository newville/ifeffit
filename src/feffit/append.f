      subroutine append(str1, str2, ilen)
c   append str2 to str1.
c   str1 and ilen are overwritten
c   str2 is not overwritten
      character*(*)  str1, str2, tmp*2048
      integer        ilen, istrln
      external       istrln
      ilen  = max(1, istrln(str1))
      tmp   = str1(1:ilen)//str2
      ilen  = min(istrln(tmp), len(str1))
      str1  = tmp(1:ilen)
      return
      end
