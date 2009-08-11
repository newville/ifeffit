       subroutine iff_print(string)
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
c generalized print
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'encod.h'
       save
       character*(*)  string, str*512, s*512, sout*2048
       double precision getsca
       integer iw, i, j, k, istrln, ntmp
       integer  ilsout, isarr, iff_eval
       logical  istext, islit, needcr
       external istrln, iff_eval, getsca

       if (int(getsca('&sync_level',0)).ge.2)  call iff_sync
       str = string
       needcr = .false.
c  interpret any and all keyword/value pairs for setting options
       nkeys = mkeys
       call print_arg(str, '''','''', 1, nkeys, keys)
c
       messg  = blank
       sout   = blank
       ilsout = 1
       do 900 iw = 1, nkeys  
          k = istrln(keys(iw))
          s = keys(iw)(1:k)
          call lower(s)
          call triml(s)
          k = istrln(s)
          istext = (s(1:1).eq.'$')
          islit  = ((s(1:1).eq.'''') .and.(s(k:k).eq.''''))
c     $         .or. ((s(1:1).eq.'''').and.(s(k:k).eq.'''')))
c
c literal string
          if (islit) then
             write(messg,'(1x,a,1x)') s(2:k-1)
             call ipr_app(sout,messg,ilsout,0)
             needcr = .true.
c
c text string
          elseif (istext) then
             do 120 j = 1, maxtxt
                if (s(2:).eq.txtnam(j)) then
                   i = istrln(text(j))
                   write(messg,'(1x,a,1x)') text(j)(:i)
                   call ipr_app(sout,messg,ilsout,0)
                   needcr = .true.
                   go to 125
                end if
 120         continue 
 125         continue 
c
c numerical variable: evaluate
          else 
             isarr = iff_eval(s(1:k), undef, tmparr, ntmp)
             if (isarr.eq.1) then 
                do 780  j =1,ntmp
                   write(messg,'(1x,g17.9)') tmparr(j)
                   call ipr_app(sout,messg,ilsout,0)
                   needcr = .true.
 780            continue 
             else
                write(messg,'(1x,g17.9)') tmparr(1)
                call ipr_app(sout,messg,ilsout,0)
                needcr = .true.
             end if 
          end if
c
 900   continue 
       if (needcr)  call ipr_app(sout," ",ilsout,1)
       return
       end


       subroutine ipr_app(str, s2, ilen,iflush)
c
c
       character*(*) str, s2
       integer  istrln, ilen, iflush
       external istrln

       str  = str(1:ilen)//s2
cc       call triml(str)
       ilen = istrln(str)
       if ((iflush.eq.1).or.(ilen.ge.128)) then
          if (ilen.ge.1) call echo(str)
          str  = ' '
          ilen = 1
       endif
       return 
       end

       subroutine print_arg(str,s1,s2,iblank,nwords, words)
c
c   breaks string into arguments =  words are separated by one or
c   more whitespace, unless "protected" by quotes, braces, or parens. 
c   the protecting characters are stripped off
c
c     args        i/o      description
c     ----        ---      -----------
c     s            i       char*(*) string to be broken up
c     s1           i       open  delimiters to preserve on output
c     s2           i       close delimiters to preserve on output
c     iblank       i       flag for whether to break words on whitespace
c                          (1 yes, 0 comma-only)       
c     nwords      i/o      input:  maximum number of words to get
c                          output: number of words found
c     words(nwords) o      char*(*) words(nwords)
c                          contains words found.  words(j), where j is
c                          greater then nwords found, are undefined on
c                          output.

       integer  i, ibeg, ilen, istrln, icount, iblank
       character*(*) str, s1, s2, words(nwords)
       character s, blank, comma, op*4, cl*4, st1, st2
       logical   comfnd, delims, isquot, border
       parameter (blank = ' ', comma = ',')
       external istrln
       data op, cl / '[{"''',  ']}"'''/

       delims = (s1.ne.blank).or.(s2.ne.blank)
cc       if (s1.ne.blank)  op = s1
cc       if (s2.ne.blank)  cl = s2
       mwords = nwords
       nwords = 0
       call untab (str)
       call triml (str)
       ilen   = istrln (str) + 1
       str    = str//' '
c  check for blank string
       if (ilen .eq. 1) return
       ibeg   = 1
       iprot  = 0
       comfnd = .true.
       isquot = .false.
       i = 0
 100   continue 
       i  = i + 1
       s  = str(i:i)
       if (nwords .ge. mwords)  return
c
c
       border =(s.eq.comma).or.((iblank.eq.1).and.(s.eq.blank))
c if the string is protected, march on til end of protection
       if (index(op,s).ge.1) then
          comfnd = .false.
          ibeg   = i 
          iprot  = index(op,s)
          icount = 0
          if (iprot.gt.0) then
             icount = 1
             st1    = op(iprot:iprot)
             st2    = cl(iprot:iprot)
             isquot = st1.eq.st2
          endif
 120      continue 
          i = i + 1
          s  = str(i:i)
          if (isquot) then 
             if (s.eq.st1) icount = 0
          else
             if (s.eq.st1) icount = icount + 1 
             if (s.eq.st2) icount = icount - 1
          end if
          if ((i.le.ilen).and.(icount.ge.1)) go to 120
          nwords = nwords + 1
          words (nwords) = str (ibeg: i)
cc          print*, ':: nwords ', nwords, ' = ', str(ibeg:i)
          
c  if 'protected delimiters' was not used, strip off first and
c  last characters
          if (.not.delims) words(nwords) = str(ibeg+1:i-1)
          go to 100
c
c if not a comma or blank, we have a word: continue til next blank or comma
c
       elseif (.not.border) then
          comfnd = .false.
          ibeg  = i
 160      continue 
          i = i + 1
          s  = str(i:i) 
          if ((i.le.ilen).and.(s.ne.comma).and.(s.ne.blank)) go to 160
          nwords = nwords + 1
          words (nwords) = str (ibeg: i-1)
          go to 100
c
c keep track of double commas -- count as a blank word
c
       elseif (s.eq.comma) then
          if (comfnd) then
             nwords = nwords + 1
             words (nwords) = blank
          endif
          comfnd = .true.
       end if
       if (i.le.ilen) go to 100
c
       return
c end subroutine gtarg
       end




