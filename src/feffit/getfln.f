       subroutine getfln(strin, filnam, ierr)
c  strip off the matched delimeters from string, as if getting
c  a filename from "filename", etc.
       integer idel, iend, istrln, ierr
       character*(*) strin, filnam, tmp*144, ope*8, clo*8
       data ope, clo /'"{(<''[',  '"})>'']'/
c
       ierr  = 0
       tmp   = strin
       call triml(tmp)
       ilen  = istrln(tmp)
       idel  = index(ope,tmp(1:1))
       if (idel.ne.0) then
          iend = index(tmp(2:), clo(idel:idel) )
          if (iend.le.0) then
             ierr = -1
             iend = ilen
          end if
          filnam = tmp(2:iend)
       else
          iend = index(tmp,' ') - 1
          if (iend.le.0) iend  = istrln(tmp)
          filnam = tmp(1:iend)
       end if
       return
c end  subroutine getfln
       end

