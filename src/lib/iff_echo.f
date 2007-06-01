       subroutine iff_echo(string,ipause)
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
c purpose: print string, optionally waiting for user input
c
c arguments:
c      string  line to write as message               [in]
c      ipause  integer flag for pausing (1=pause)     [in]
c
c   1. for 'pause' mode, the default message (ie when string='')
c          is ' -- hit return to continue -- '
c   2. uses  non-standard format(1x,a,$) statement
c
c requires: istrln, undels, echo, chrdmp
       implicit none
       character*(*)  string, str*256, c*1, msgdef*64
       parameter (msgdef = ' --  hit return to continue -- ')
       double precision getsca
       integer ilen, istrln, ipause, i_ec, i_pa
       external istrln,  getsca
c
       c   = ' '
       str = string
       call undels(str)
       call triml(str)
       i_ec = int(getsca('&screen_echo',0))
       i_pa = int(getsca('&pause_ignore',0))
       if ((i_ec.eq.1).and.(i_pa.eq.0).and.(ipause.eq.1)) then 
          if (str.eq.' ') str = msgdef
          ilen = istrln(str)
          call chrdmp(str(1:ilen))
          read(*,'(a)') c
       else
          ilen = max(1,istrln(str))
          call echo(' '//str(1:ilen))
       end if
       return
       end
