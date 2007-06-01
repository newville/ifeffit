       subroutine iff_getline(str, key, val, arg, ilen)
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
c purpose:  manange a single full command line from multi-line
c           inputs, and parse input line to do the following:
c           -1-  determine keyword, value, argument:
c                if the first word is in the list _ckeys_, then
c                _key_ is set to that, _arg_ is the rest of the
c                line, and _val_ is the first word of _arg_.
c                if the first word is not in _ckeys_, then 
c                _key_ = 'def' and _arg_ = whole line.
c                ckeys held in common /comkey/
c           -2-  manage a long command, over multiple input lines:
c                if a _key_ is followed by '(', the command line
c                will not terminate until an "unprotected" ')'.
c           -3-  remove end-of-line comments.
c
c arguments:
c      str      next input line                         [in]
c      key      keyword (1st word)                      [out]
c      val      1st word of arg                         [out]
c      arg      argument (everything except 1st word)   [out]
c      ilen     length of output line                   [in/out]
c               on output, ilen<0 if line is to be continued
c
c notes:
c   1. handling multi-line commands:
c      a) a line will be continued to the next line if the
c         full line contains unmatched (unprotected) parens.
c      b) if a line is too be continued, ilen is set to -ilen
c
c   2.  '!', '#', and '%'  are end-of-line comments, but can be
c       protected by matching " ", ' ', or { }.
c
c   3. setting ilen=0 will initialize a bunch of stuff, and
c      is probably desirable on first call.
c
c     If on arriving at Trude I had not read the city's name written
c     in big letters, I would have thought I was landing at the same
c     airport from which I had taken off. ... Why come to Trude?  I
c     asked myself. And I already wanted to leave.
c
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       character*(*)   str, val, arg, key
       character*2048  line, tmp, words(2), tmpkey
       character       s*1, copen*3, cclos*3, ceol*3
       integer         itmp, istrln, ilen, i, iprot
       integer         iparen,ikey,nwords, ip
       logical         newlin, do_final_trim
       common /icomnd/ iprot, iparen, itmp, newlin
       common /ccomnd/ tmp, tmpkey, copen, cclos, ceol
       save
       external istrln
c  initialization
       arg = blank
       key = blank
       val = blank
       if (ilen.eq.0) then
          newlin = .true.
          iprot  = 0
          iparen = 0
          ilen   = 0
          tmp    = blank
          tmpkey = blank
          copen  = '{''"'
          cclos  = '}''"'
          ceol   = '!#%'
       end if
c
c  clean up input line
       line = str
       call sclean(line)
       call triml(line)
       ilen = istrln(line)
       if (ilen.lt.0) then
          ilen = -itmp
          return
       end if
c
c if we're getting a new line :
       if (newlin) then
          tmpkey = blank
          tmp    = blank
          iprot  = 0
          iparen = 0
          itmp   = 1
c      get the candidate keyword, and determine if the line is
c      a "single-line" or "multi-line" command
          ikey  = 0
          do 20 i = 1, ilen
             s = line(i:i)
             if (s.eq.'(') then
                ikey  = i - 1
                go to 22
             elseif ((s.eq.blank).and.(ikey.eq.0) ) then
                ikey = i - 1
             elseif ((s.ne.blank).and.(ikey.ne.0)) then
                go to 22
             end if
 20       continue
          ikey = ilen
 22       continue
c      now check the candidate keyword against the known list
c      if not found, key is set to 'def', and the whole line is kept
          key = line(1:ikey)
          call lower(key)
          do 30 i = 1, mckeys
             if (key.eq.ckeys(i))  go to 65
 30       continue
          do 40 i = 1, macmax
             if (key.eq.macnam(i)) go to 65
 40       continue
          key   = undef
          ikey  = 0
 65       continue
          tmpkey= key
          line  = line(ikey+1:ilen)
          call triml(line)
          ilen  = istrln(line)
       end if
c
c remove unprotected end-of-line comments, update paren count
       do 150 i = 1, ilen
          s = line(i:i)
c      keep track of whether this part of the string is "protected"
          if (iprot.eq.0) then
             iprot = index(copen,s)
          elseif (iprot.le.3)  then
             if (s.eq.cclos(iprot:iprot)) iprot = 0
          end if
c     look for end-of-line comments
          if ((iprot.eq.0).and.(index(ceol,s).ne.0)) then
             line = line(1:i-1)
             call triml(line)
             go to 160
          end if
          if (s.eq.'(') iparen = iparen + 1
          if (s.eq.')') iparen = iparen - 1
 150   continue
 160   continue
       ilen = istrln(line)
c  if multi-line, check for final ')'.
c  and if it's found, peel of the enclosing ( ... )
c
c  the line is not complete:
       if (iparen.gt.0) then
          tmp   = tmp(1:itmp)//line(1:ilen)
          call triml(tmp)
          itmp  = istrln(tmp) + 1
          ilen  = -itmp
          newlin= .false.
c  the line is complete:
       else
          arg    = tmp(1:itmp)//line(1:ilen)
          call triml(arg)
          ilen   = max(1, istrln(arg))
          if ((arg(1:1).eq.'(').and.(arg(ilen:ilen).eq.')')) then
             ip  = 0
             do_final_trim = .true.
             do 350 i = 2, ilen-1
                s = arg(i:i)
                if (s.eq.'(') ip = ip + 1
                if (s.eq.')') ip = ip - 1
                if (ip.lt.0)  do_final_trim = .false.
 350         continue 
             if (do_final_trim) then
cc                print*, ' getline removing surrounding  parens'
                arg = arg(2:ilen-1)
                call triml(arg)
                ilen = max(1, istrln(arg))
             endif
          end if
          newlin = .true.
          key    = tmpkey
          if ((key.eq.blank).or.(key.eq.undef)) key = def_command
          nwords = 2
          call bwords(arg,nwords,words)
          val    = words(1)
       end if
c
c  end subroutine iff_getline
       return
       end
