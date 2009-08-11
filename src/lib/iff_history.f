       subroutine iff_history(str)
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
c manage history of commands for ifeffit
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       save
       character*(*) str, argu*512, defkey(2)*64
       integer  i, ndfkey, k, istrln
       external istrln
       argu = str
c
       call bkeys(str, mkeys, keys, values, nkeys)
       ndfkey    = 1
       defkey(1) = 'file'
       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if ((values(i).eq.undef).and.(keys(i).ne.'off')
     $         .and.(i.le.ndfkey)) then
             values(i) = keys(i)
             keys(i)   = defkey(i)
          end if
          if (keys(i).eq.'off') then
             close(iohist)
             histry = .false.
             call settxt('historyfile',' ')
          elseif (keys(i).eq.'file') then
             tmpstr = values(i)
             call settxt('historyfile',tmpstr)
             call newfil(tmpstr, iohist)
             histry = .true.
             if (iohist.le.0) then
                call warn(3,'bad open of history file')
                histry = .false.
             end if
          elseif (histry)  then
             call gettxt('historyfile',tmpstr)
             messg = ' history file = ' // tmpstr
             call echo(messg)
          else
             call echo(' history is off')
          end if
 100   continue 
       tmpstr = ' '
       return
       end
