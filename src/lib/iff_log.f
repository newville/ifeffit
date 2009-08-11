       subroutine iff_log(string)
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
c  turn logging on/off for ifeffit
c
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'echo.h'
       save
       character*(*)  string, str*512, defkey(2)*64
       double precision getsca , dec
       integer   iex, ier, iec, k, i, il, istrln, ndfkey
       logical   dopen, dclos
       external  istrln, getsca
       iec   = int( getsca('&screen_echo',0) )
cc       print*,'  log():  screen_echo = ' , iec, i_echo
       dopen = .false.
       dclos = .false.
       str   = string
       call bkeys(str, mkeys, keys, values, nkeys)
c
c set default keywords for "undefined" sets
c ie: the first two unkeyed values will take keys 'energy', 'xmu'
        ndfkey    = 1
        defkey(1) = '_un_'
        do 100 i = 1, nkeys
           k = istrln( keys(i))
           if ((values(i).eq.undef).and.(i.le.ndfkey)) then
              values(i) = keys(i)
              keys(i)   = defkey(i)
           end if
           if (keys(i).eq.'file') then 
              echo_file = values(i)
              dopen   = .true.
           elseif (keys(i).eq.'close') then
              dclos = .true.
           elseif (keys(i).eq.'_un_') then
              if (values(i).eq.'close') then
                 dclos = .true.
              else
                 echo_file = values(i)
                 dopen   = .true.
              endif
           else
              echo_file = values(1)
              dopen = .true.          
           endif
 100    continue 
c
cc        print*, ' log(): dec = ' , i_echo
c  if needed, open the log file
        il = istrln(echo_file)
        if (dclos .and. (lun_echo .ge. 1)) then 
           close(lun_echo)
cc           print*, ' closed log file ', echo_file(1:il)
           lun_echo  = -1
           echo_file = ' '
           dopen     = .false.
           i_echo = i_echo - 2
        endif
        if (dopen) then
           if (lun_echo.ge.1) then
              close(lun_echo)
              lun_echo = -1
           endif
           lun_echo = 1
           call triml(echo_file)
           call openfl(lun_echo, echo_file, 'unknown', iex, ier)
cc           print*, ' opened log file ', echo_file(1:il)
           i_echo = i_echo + 2
        end if
c
c determine what screen echo level was requested:
        dec   = i_echo * one
        call setsca('&screen_echo', dec)
        return
        end        









