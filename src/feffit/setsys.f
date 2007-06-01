       subroutine setsys(system,vaxflg,dosflg,macflg)
c simple way of setting flags, describing the operating system used.
c rather than setting all flags by hand, this uses a single string
c and ensures that only one flag is on
       character*(*) system, sys*3
       logical       vaxflg,dosflg,macflg
       vaxflg = .false.
       dosflg = .false.
       macflg = .false.
       call triml(system)
       call smcase(system,'a')
       sys = system(:3)
       if ((sys.eq.'vax').or.(sys.eq.'vms')) then
          vaxflg = .true.
          system = sys
       elseif (sys.eq.'mac') then
          macflg = .true.
          system = sys
       elseif (sys.eq.'dos') then
          dosflg = .true.
          system = sys
       else
          system = 'unix'
       endif
       return
       end
