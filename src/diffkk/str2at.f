       subroutine str2at(str,iz)
c  given a string str that contains either an atomic number or
c  an atomic symbol, return the atomic number to iz.
c
       character*(*) str 
       integer  iz, ierr, iatsym
       logical  isnum
       external isnum, iatsym
       if (isnum(str))   then
          call str2in(str,iz,ierr)
          if (ierr.ne.0) iz = 0
       else
          iz = iatsym(str)
       end if
       return
c      end subroutine str2at
       end
