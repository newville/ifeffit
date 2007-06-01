       subroutine  iff_macro_def(str)
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
c define a macro:
c  returns 0 if macro definition is done, 1 if it's still expecting more lines
c  also uses mac_define array for control
c
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       save

       integer   nwords, i, j, jmac, jtmp, istrln, mwordx, immac
       parameter (mwordx = 12)
       character*(*) str , line*256
       character*64  words(mwordx), op*3
       double precision getsca, x
       external istrln, getsca
       data op / '{"'''/
       nwords = mwordx
       line   = str
c
c if mac_define(1) is 1, then we're reading lines for a defined macro
       if (mac_define(1).ge.1) then
          immac = mac_define(4)
          call sclean(line)
          call triml(line)
          if (mac_define(1).eq.1) then
             mcdesc(immac) =  undef
             if (index(op,line(1:1)).ne.0) then
                call undels(line)
                mcdesc(immac) = line
                line = ' '
             end if
cc             print*, ' macro ', macnam(immac)(1:20)
cc             print*, ' descr ', mcdesc(immac)(1:45)
          end if
          mac_define(1) = mac_define(1) + 1
          jtmp = mac_define(2)
          jmac = mac_define(3)
          if (line.ne.' ')  then
             if (line(1:9).eq.'end macro') then
                imcptr(jtmp)  = -1
                mac_define(1) = -1
                return 
             else
                macstr(jmac) = line
                imcptr(jtmp) = jmac
c      find next available line
                jtmp = jmac
 50             continue
                jmac = jmac + 1
                if (jmac.ge.mcline)  then
                   mac_define(1) = -1
                   return 
                end if
                if (imcptr(jmac).ne.0) go to 50
             end if
          end if
          mac_define(2) = jtmp
          mac_define(3) = jmac
          return 
       endif
c
c if mac_define(1) = 0, then we're setting up the macro _name_ line
c

       call bwords(line,nwords,words)
c  find next blank macro space, and erase any macro with the same name
       do 300 i = 1, macmax
          if (macnam(i).eq.' ') go to 350
c  for each macro i:  macnam(i) = macro name, imacro(i) = 1st macro line
c  then, starting at j = imacro(i), macstr(j) = next macro line,
c  and the next  j = imcptr(j).
c
c  add syntax check: that macro name cannot = other command name!
          if (macnam(i).eq.words(1)) then
             j        = imacro(i)
 200         continue
             jtmp     = imcptr(j)
             macstr(j)= undef
             imcptr(j)= 0
             if (jtmp.gt.0) then
                j = jtmp
                go to 200
             end if
             go to 350
          end if
 300   continue
       i    = i - 1
 350   continue
       jmac = i
c save macro name 
       macnam(jmac) = words(1)
cc       print*, ' mac: ', jmac, ' name = ', macnam(jmac)(:20)
       mac_define(4) = jmac
c get macro arguments
       if (nwords.ge.2) then
          tmpstr = line
          call strclp(tmpstr,words(1),words(2),line)
          nwords = mwordx
          call gtarg(line,blank,blank,1,nwords,words)
cc          print* ,  'args of ', line(1:70)
          do 380 i = 1, nwords
cc             print* , i, ' -> ', words(i)
             mcargd(jmac,i) = words(i) 
             call rmquot(mcargd(jmac, i) )
             call rmdels(mcargd(jmac, i) ,'{','}')
 380      continue 
       end if
c      
c ok, now we're ready to start the new macro       
c  get index of the first line for this macro in the macro array
       jtmp = 0
 450   continue
       jtmp = jtmp + 1
       if (jtmp.gt.mcline)    return
       if (imcptr(jtmp).ne.0) go to 450
       imacro(jmac) = jtmp
c
c jtmp stores "previous index", jmac the "current index".
c if the current line (macstr(jmac)) isn't "end macro",
c then imcptr(jtmp) = jmac.   otherwise imcptr(jtmp) = -1
       jtmp = macmax
       jmac = imacro(jmac)
c set mac_define(1) to  1 to flag that we're waiting
c for more macro lines
       x = getsca('&screen_echo',0)
       if ((x.gt.0.5) .and.  (ioinp.le.0))
     $      call echo('   enter macro lines, ending with "end macro"')
       mac_define(1) = 1
       mac_define(2) = jtmp
       mac_define(3) = jmac
       return 
c end routine iff_macro_def
       end


       subroutine iff_macro_do(indmac, str, ipflag,wrthis)
c execute a macro, including recursive macro calls
c (this exits when _all_ macros have processed)
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       character*(*)   str, arg*2, key*128
       character*2048  com, argu, val 
       logical  wrthis, isamac
       integer istrln, i, j, iffcmd_exec, nword, indmac, ifeffit
       integer isarg, ilen, ipflag, italo, jmac

       external istrln, iffcmd_exec, ifeffit, isamac
       save
c initialization
cc       print*, 'iff_macro_do: indmac, ipflag= ', indmac, ipflag
       ilen   = istrln(str)
       
       iprint   = ipflag
       mac_exec = 1
       imac     = imacro(indmac)
       
       if (nmacro.lt.0)     nmacro = 0
       if (nmac_stop.lt.0)  nmac_stop = 0
       nmacro   = nmacro + 1
       call iff_set_macargs(indmac,str)

 100   continue
       if (nmacro.gt.mcdeep)  then
          call warn(2,'too many macros')
          return
       elseif (nmacro.le.0) then
          return
       endif
       italo  = -99
       com    = undef
       if ((imac.gt.0).and.(imac.le.mcline)) then 
          italo = imcptr(imac)
          com = macstr(imac)
       endif
c insert current macro argument values
 180   format(a,i1)
       do 200 i = 1, mmcarg
          write(arg,180) '$',i
 190      continue 
          isarg = index(com,arg)
          if (isarg.ne.0) then
             ilen = istrln(mcargs(nmacro,i))
             com  = com(1:isarg-1)//mcargs(nmacro,i)(1:ilen)
     $            //com(isarg+2:)
             go to 190
          end if 
 200   continue 
c  get keyword/argument from command line
       call iff_getline(com, key, val, argu, ilen)
       if ((key.eq.'def').and.(argu.eq.' ')
     $      .and.(ilen.ge.0)) ilen = 0

       if (ilen.gt.0) then
c determine if this is a call to another macro
          if (isamac(key,jmac)) then
             imac_save(nmacro)= imcptr(imac)
             nmacro           = nmacro + 1
             imac             = imacro(jmac)
             call iff_set_macargs(jmac,argu)       
             go to 100
          else
             italo = iffcmd_exec(key,argu,wrthis)
             italo = 0
             if (italo.eq.1) then
                call iff_done
                stop
             endif
          endif
       endif

c get index of next macro line
       if (imac.ge.0) then
          imac = imcptr(imac) 
       elseif (imac.eq.-1)  then
          nmacro  = nmacro - 1
          if (nmacro.le.nmac_stop) return
          imac  = imac_save(nmacro)
       endif
       go to 100
c done
       mac_exec = 0
       return 
       end

       subroutine gtarg(str,s1,s2,iblank,nwords, words)
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
       character s, blank, comma, op*6, cl*6, st1, st2
       logical   comfnd, delims, isquot, border
       parameter (blank = ' ', comma = ',')
       external istrln
       data op, cl / '[{"((''',  ']}"))'''/

       delims = (s1.ne.blank).or.(s2.ne.blank)
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

       subroutine iff_set_macargs(indmac,str)
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       character*(*) str, words(mmcarg)*64
       integer i, ilen, istrln, nword, indmac

cc get macro arguments
       nword  = mmcarg
       do 50 i = 1, mmcarg
          words(i) = ' '
 50    continue
       call gtarg(str,blank,blank,1,nword,words)
cc       if (iprint.ge.8)  print*,  'macro args: ', str(1:30)
       do 60 i = 1, mmcarg
ccc          print* , i, ' -> ', words(i)
          mcargs(nmacro,i) = words(i) 
          if (words(i).eq. ' ') then
             mcargs(nmacro,i) = mcargd(indmac,i)
          else
             mcargs(nmacro,i) = words(i)
          end if
          call rmquot(mcargs(nmacro,i) )
          call rmdels(mcargs(nmacro,i) ,'{','}')
          if ((iprint.ge.8).and.(words(i).ne.' ')) then
             ilen = istrln(mcargs(nmacro,i))
cc             print*, 'macro arg ', i, ':',mcargs(nmacro,i)(:ilen), ':'
          end if
 60    continue 
       return
       end
