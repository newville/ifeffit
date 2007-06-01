       integer function iff_load(str)
c
c//////////////////////////////////////////////////////////////////////
c Copyright (c) 1997--2000 Matthew Newville, The University of Chicago
c Copyright (c) 1992--1996 Matthew Newville, University of Washington
c
c Permission to use and redistribute the source code or binary forms of
c this software and its documentation, with or without modification is
c hereby granted provided that the above notice of copyright, these
c terms of use, and the disclaimer of warranty below appear in the
c source code and documentation, and that none of the names of The
c University of Chicago, The University of Washington, or the authors
c appear in advertising or endorsement of works derived from this
c software without specific prior written permission from all parties.
c
c THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
c EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
c MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
c IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
c CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
c TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
c SOFTWARE OR THE USE OR OTHER DEALINGS IN THIS SOFTWARE.
c//////////////////////////////////////////////////////////////////////
c
c support "load file" syntax for ifeffit:
c    1. manage stack of files to read from
c    2. check for recursion  (is it already on the stack)
c    3. open file
c    4. execute each line (by sending to ifeffit)
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'echo.h'
       save
       integer   iex, ier, i, ifeffit, ilen, istrln, iread
       character str*(*), stat*10
       character*2048  line
       parameter (stat = 'old')
       external  ifeffit, istrln, iread
c
c increment stack -- check that we're not off the edge
       ier      = 0
       iff_load = 0
       nfiles   = nfiles + 1
cc       print*, 'LOAD: iff_load  ', nfiles
       if (nfiles .gt. mfiles) then
          call warn(2,'# ifeffit error: too many nested "include"s')
          write(messg, '(a,i3)') '# current limit is ', mfiles
          call echo(messg)
          call warn(2,'# file include will be ignored ')
          nfiles = nfiles - 1 
          iff_load = -1
          return
       end if
c  fix string to be file name
cc       print*, 'LOAD: string ', str
       call getfln(str, lfiles(nfiles), ier)
c  test for recursion:
       do 20 i = 1, nfiles - 1
          if (lfiles(nfiles) .eq. lfiles(i)) then
             call warn(3,
     $            '# ifeffit error: recursive include not allowed')
             iff_load = -1
             return
          end if
 20    continue
c  open file
cc       print*, 'call openfl nfiles =', nfiles, lfiles(nfiles)(1:30)
       call openfl(iunit(nfiles), lfiles(nfiles), stat, iex, ier)
       if ((ier.lt.0).or.(iex.lt.0)) then
          call echo(' *** load: error opening file (not found):')
          ilen = istrln(lfiles(nfiles))
          messg =   ' ***    '//lfiles(nfiles)(1:ilen)
          call warn(2,messg)
          go to 500
       end if
       ioinp = iunit(nfiles)
cc       print*, ' iff_load:: opened unit = ', ioinp, nfiles ,
cc     $      lfiles(nfiles)(1:30)
c  read first line, check if this is really a save file
       ilen = iread(iunit(nfiles), line)
       if (line(1:22).eq. '#:IFEFFIT SAVE File: v') then
          call iff_restore(lfiles(nfiles))
          go to 500
       endif
c  while loop:  read and execute
 100   continue 
       if (nfiles.gt.0) then
          if (ilen.gt.0) then
             iff_load = ifeffit(line)
             if (iff_load.eq.1)  return 
          endif
          ilen = iread(iunit(nfiles), line)
          if ((ilen.lt.0) .and. (nfiles.gt.0)) then
cc             print*, ' close file ', lfiles(nfiles)(1:40)
             close(iunit(nfiles))
             lfiles(nfiles) = ' '
             nfiles  = nfiles - 1
             ioinp   = 0
          endif
          go to 100
       end if
c  at end-of-file, pop stack.  continue with previous loads, or return
 500   continue
cc       print*, ' line 500 : ', nfiles
       if (nfiles.gt.0) then
          close(iunit(nfiles))
          lfiles(nfiles) = ' '
          nfiles  = nfiles - 1
          ioinp   = 0
       end if
cc       print*, ' nfiles is now ', nfiles
       if (nfiles.gt.0) then 
          ilen = iread(iunit(nfiles), line)
          go to 100
       endif

       do 900 i = 1, n_echo
          call echo_pop(tmpstr)
 900   continue 
       n_echo = 0
       call setsca('&echo_lines', zero)
       return
c  end subroutine iff_load
       end

