       subroutine iff_set(key,str,wrthis)
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
c purpose: set program variables for ifeffit:
c          scalars, arrays,  strings, string arrays
c arguments:
c      key     command to perform (def, set, guess)     [in]
c      str     command line for ifeffit                 [in]
c
c notes:
c   1. needed changes:
c         a) add string arrays (for titles lines, plot labels, ???)
c         b) support guess
c
c requires:  everything in ifeffit
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'encod.h'
       include 'echo.h'
       save
       character*(*)  key, str, prefix*256,  tmpnam*64
       character*2048 formula, bxvals(mkeys)
       integer  iofarr, iofsca, istrln, ilen, iltmp
       integer  icode(micode) , jcode(micode), encod, i_sync
       integer  i, j, k, ntmp, idot, ier, ik, isset, isgues, ix
       integer xafs_path, iup, ret
       double precision tmp2(maxpts), tmpx, getsca
       logical  isarr, isnum, istext, s_is_n, trivial
       logical  isvnam, wrthis,lkeep, ic_is_arr
       external iofarr, iofsca, istrln, isnum
       external isvnam, ic_is_arr, encod, getsca, xafs_path

c find the right thing to evaluate:
c  1. is it something that takes a string value, or a numeric value?
c  2. if numeric, is it an array or a scalar?
c      if not, assume it will be an array, -- we'll check later
c      look for an array with the same name, or the first empty array
c       print*, 'iff_set ', key(1:3), mac_define(1)
       call gettxt('group', prefix)
       call bkeys(str, mkeys, keys, bxvals, nkeys)
c  GUESS / SET / DEF
       if (int(getsca('&sync_level',0)).ge.6)  call iff_sync
       isgues =  0
       isset  =  0
       if (key.eq.'guess') isgues = 1
       if (key.eq.'set')   isset  = 1
       ix = 0
c
 100   continue 
       ix      = ix + 1
       if (ix.gt.nkeys) return
       ntmp   = maxsize_array
       ilen   = istrln(bxvals(ix))
       s_is_n =  isnum(bxvals(ix)(1:ilen))
ccc    formula= '('//bxvals(ix)(1:ilen)//')'
       formula=  bxvals(ix)(1:ilen)

       tmpnam = keys(ix)
       iltmp  = istrln(tmpnam)

cc       print*, ' iff_set: ', tmpnam(:iltmp), ' -> ', formula(1:ilen)

       if ((iltmp.le.0).or.(tmpnam.eq.undef)) then 
          call echo(' Warning -- command ignored (typo?):')
          call warn(1,'     -> '//keys(ix)(1:ik))
          go to 100
       endif
       istext = tmpnam(1:1).eq.'$'
c if the value is undefined, then the string was almost certainly 
c a typo that got misinterpreted by getlin to be "set gibberish"
       if (bxvals(ix).eq.undef) then
          ik = istrln(keys(ix))
          call echo(' Warning -- command ignored (typo?):')
          call warn(1,'     -> '//keys(ix)(1:ik))
          wrthis = .false.
          go to 100
       end if
c
c    is the value defined?
c    is this a text variable?
       if (istext) then
          if (.not.isvnam(tmpnam,3)) then
             ik = istrln(tmpnam)
             call warn(1,' Invalid text string name: '//
     $            tmpnam(1:ik) )
             go to 100
          end if
          if (tmpnam.eq.'$&build') then
             call warn(2,' can''t reset value of $&build')
             go to 100
          endif
cc          call str_eval(bxvals(ix), messg)
cc          print*, ' messg = ', messg(1:20)
          call settxt(tmpnam(2:),bxvals(ix))
          return
c    check for immutable values, like pi:
       elseif (tmpnam.eq.'pi') then
          call warn(2,' can''t reset value of pi')
          go to 100
       elseif (tmpnam.eq.'etok') then
          call warn(2,' can''t reset value of etok')
          go to 100
c    this is a numeric variable (scalar or array and maybe "guess"ed)
       else
c
c   first, check that it's not a macro or command name
          do 280 i = 1 , mckeys
             if (ckeys(i).eq.tmpnam) then 
                ik = istrln(tmpnam)
                call warn(2,' Invalid variable name: '//
     $               tmpnam(1:ik) // '  is a command name')
                go to 100
             end if
 280      continue 
          do 290 i = 1 , macmax
             if (macnam(i).eq.tmpnam) then 
                ik = istrln(tmpnam)
                call warn(2,' Invalid variable name: '//
     $               tmpnam(1:ik) // '  is a macro name')
                go to 100
             end if
 290      continue 
          if (.not.isvnam(tmpnam,-1)) then
             ik = istrln(tmpnam)
             call warn(2,' Invalid variable name: '//
     $            tmpnam(1:ik) )
             go to 100
          end if
c    so encod and decod the array
cc          print*, 'iff_set-> encode for ',s_is_n,' ',formula(1:50)
          if (s_is_n) then
cc             print*, ' call str2dp ', bxvals(ix)(1:ilen) 
             call str2dp(bxvals(ix)(1:ilen),tmparr(1), ier)
             tmparr(2) = zero 
             isarr     = .false.
             trivial   = .true.
             idot      = 0
          else
             trivial   = .false.
cc             print*, ' - encod : ', formula(1:40)
             ier = encod(formula, jcode, icode)
             if (ier.ne.0)  goto 700
             if (iprint.ge.1) call rpndmp(jcode)
             tmparr(1) = zero
             tmparr(2) = zero
cc             print*, ' - decod'
             call decod(icode, micode, consts, scalar, array,
     $            narray, nparr, maxsize_array, maxarr, ntmp, tmparr)
cc             print*, 'iff_set: ntmp = ', ntmp
c    now check whether this should have really been a scalar
c    that is, does the array refer to other arrays??
             isarr = ic_is_arr(icode,micode).and.(ntmp .gt. 1)
             isarr = isarr.and.(isgues.eq.0)
             idot = index(tmpnam,'.')
cc             print*, ' isarr ', ic_is_arr(icode,micode), ntmp
          endif
cc          print*, 'iff_set: isarr, isset = ', isarr, isset
c    this is an array
          if (isarr) then
cc             print*, ' - set_array ', ntmp
             call set_array(tmpnam, prefix, tmparr, ntmp, 1)
             k    = iofarr(tmpnam,prefix,0,0)
             if (k.lt.0) then 
                call echo(' *** error: could not find variable')
                tmpstr = ' *** trying to define '//tmpnam
                call warn(2,tmpstr)
                return
             else if (k.eq.0) then
                call warn(3,' *** error: out of memory')
                return
             endif
             arrfrm(k) = ' '
c
c  here (and only here) do we 'save the formula' of an array
             if (isset.eq.0) then
                arrfrm(k) = formula
                do 440 j = 1, micode
                   icdarr(j,k) = icode(j)
 440            continue
             endif
c             print*, ' saving formula for ', k, ' -> ', tmpnam(1:40)
c             print*, ' == ', arrfrm(k)(1:40)
c             call rpndmp(jcode)
c             call rpndmp(icdarr(1,k))
             call fixarr(k,tmpnam,ntmp,isset)
c 
c  this is a scalar
          else
             idot = index(tmpnam,'.')
cc             print* ,' value is a scalar ', idot, tmpnam
             if (idot.ne.0) then
                k  = istrln(tmpnam)
                ik = istrln(formula)
cc                call rpndmp(icode)
                
                call echo(' ** cannot set scalar value to array **')
                call warn(2,'   '//tmpnam(1:k)//' = '//formula(1:ik))
                return
             end if
c    look up str in list of scalar names
             k = iofsca(tmpnam, 1)
             scanam(k) = tmpnam
             scafrm(k) = ' '
             scalar(k) = tmparr(1)
c    is this a 'set' or 'guess' or is it a 'def'
             if ((isgues.eq.1).or.(isset.eq.1)) then 
                do 460 j = 2, micode
                   icdsca(j,k) = 0
 460            continue
                icdsca(1,k) = k + jscale
                if (isgues.eq.1) icdsca(1,k) = -1
             else
c determine if this is a trivial definition 'z = 22.'
c or even sqrt(4/7), which is to say: no named Program Variables,
c     just constant numbers and operators.
                if (.not.trivial) then 
cc                   print*, ' non trivial scalar'
                   scafrm(k) = formula
                   tmpx      = zero
                   call str2dp(formula,tmpx,ier)
                   if (tmpx .ne. zero) then
                      trivial =
     $                     (abs(tmpx - tmparr(1))/tmpx .le. 1.d-6)
                   elseif(ier.eq.-999) then 
                      trivial = .true.
                      do 470 j = 1, micode
                         if (((icode(j) .ge. 1) .and.
     $                    (icode(j) .lt. jconst))) trivial =.false.
                         if ( (icode(j).eq.jdebye).or.
     $                        (icode(j).eq.jeins)) trivial =.false.
                         if (icode(j).eq.0) go to 475
 470                  continue
 475                  continue
                   endif
                endif
                if (trivial) then
                   scafrm(k) = ' '
                   do 492 j = 1, micode
                      icdsca(j,k) = 0
 492               continue
                   icdsca(1,k) = k + jscale
                   if (isgues.eq.1) icdsca(1,k) = -1
                else
cc                   print*, 'save formula'
                   do 490 j = 1, micode
                      icdsca(j,k) = icode(j)
 490               continue
                endif
             end if
          end if
c finally, check for special system vars
          if (tmpnam.eq.'&screen_echo') then
             i_echo = int(tmparr(1))
          elseif (tmpnam.eq.'path_index') then
             iup = int(tmparr(1))
             call synvar
             if (iup .gt. 0) ret = xafs_path(iup,tmparr,tmp2,tmpx)
          endif
       end if
 700   continue 
       if (ix.lt.nkeys) go to 100
c  end subroutine iff_set
       if (int(getsca('&sync_level',0)).ge.2)  call iff_sync
       return
       end

