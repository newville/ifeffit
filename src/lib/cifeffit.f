c//////////////////////////////////////////////////////////////////////
c Copyright (c) 1997--2001 Matthew Newville, The University of Chicago
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
c  routines for the c interface to ifeffit data elements
       integer function iffexecf(str)
c
c this provides a cleaner interface for calling from C
       character*(*) str
       integer ifeffit, istrln, ilen
       external ifeffit, istrln
       call sclean(str)
       ilen = istrln(str)
       iffexecf = ifeffit(str)
       return
       end

       integer function iffgetstr(inp, out)
c
c  purpose: wrapper to gettxt
       character*(*) inp, out, str*256
       integer  ilen, istrln
       external istrln
       iffgetstr = 1
       str    = inp
       call sclean(str)
       ilen   = istrln(str)
       if (str(1:1).eq.'$') then
          str  = str(2:ilen)
          ilen = ilen - 1
       end if
       call gettxt(str, out)
       iffgetstr = max(1, istrln(out))
c  end function iffgetstr
       return
       end

       integer function iffputstr(inam, istr)
c
c  purpose: put a string
c
c  copyright (c) 1998  matt newville
       character*(*) inam, istr
       character*256  nam, str
       iffputstr = 0
       str   = istr
       nam   = inam
       call sclean(nam)
       call sclean(str)
       call settxt(nam,str)
c  end function iffputsca
       return
       end

       integer function iffgetsca(inp, dx)
c
c  purpose: wrapper to getsca
c
c  copyright (c) 1998  matt newville
       character*(*) inp, str*256
       double precision dx, getsca
       external getsca
       if (int(getsca('&sync_level',0)).ge.1)  call iff_sync
       iffgetsca = 0
       str    = inp
       call sclean(str)
       dx     = dble( getsca(str,0) )
cc       print*, 'F:iffgetsca ', str(1:30), ' = ', dx
c  end function iffgetsca
       return
       end
       integer function iffputsca(inp, dx)
c
c  purpose: wrapper to setsca
c
c  copyright (c) 1998  matt newville
       character*(*) inp, str*256
       double precision dx, xx, getsca
       external getsca
       iffputsca = 0
       str   = inp
       xx    = dx
       call sclean(str)
       call setsca(str,xx)
       if (int(getsca('&sync_level',0)).ge.1)  call iff_sync
c  end function iffputsca
       return
       end

       integer function iffgetarr(inp, dx)
c
c  purpose:  look up and return array by name
       implicit none
       character*(*) inp, str*256
       integer       get_array
       double precision dx(*), getsca
       external  get_array, getsca
       if (int(getsca('&sync_level',0)).ge.1)  call iff_sync
       str       = inp
       call sclean(str)
       iffgetarr = get_array(str,' ', 0, dx)
c  end function iffgetarr
       return
       end
       integer function iffputarr(inp, inpts, dx)
c
c  purpose: wrapper to put array in ifeffit data
       implicit none
       character*(*) inp, str*256
       integer   k,  j, inpts
       double precision dx(*), getsca
       external getsca
       iffputarr = 0
       str  = inp
       call sclean(str)
       call set_array(str, ' ', dx, inpts, 1)
       if (int(getsca('&sync_level',0)).ge.1)  call iff_sync
c  end function iffputarr
       return
       end
       integer function iffgetecho(str)
c
c  purpose: wrapper to echo_pop in ifeffit
       implicit none
       include 'consts.h'
       include 'echo.h'
       save
       character*(*) str, s*512
       integer   i,  istrln
       external  istrln
       iffgetecho = 0
       s   = ' '
       str = ' '
       call sclean(str)
       if (n_echo .ge. 1)  call echo_pop(s)
       str = s
       iffgetecho = max(1, istrln(str))
c  end function iffgetecho
       return
       end

       subroutine iff_plotraise(i)
c  grwin specific: raise to top
       integer i, j
       j = i
cc       call gwshowfr(j,11)
cc       call gwshowfr(j,12)
       return
       end
