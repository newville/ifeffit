      subroutine messag(messg)
c
c  write message to  standard ouput with (1x,a) format
c
      character*(*) messg
      write(*,10)   messg
 10   format(1x,a)
      return
c end subroutine messag
      end

