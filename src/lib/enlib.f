       subroutine enchk(strout, itemp, ieqn, ierr)
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
c   some syntax checking for encod routine
       include 'encod.h'
       character*2048 strout, emsg*512
       character synerr*22
       integer   itemp(*), ieqn, ilen, istrln, iparen
       integer   ierr, i, ibfr, iaft, it, j, jt, jstack, jcomma
       integer   jcoms(100), jsstak
       logical   strok
       external  istrln
       data synerr /' syntax error: '/


       ierr = 1
       ilen = istrln(strout)
c--
c-- check that one component math functions are followed by "("
c-- and that parentheses are not left hanging
       iparen = 0
       jsstak = 0
       do 4400 i = 1, ieqn + 2
          if (i.eq.1) then
             ibfr = ileft
          else
             ibfr = itemp(i-1)
          end if
          iaft = itemp(i+1)
          it   = itemp(i)
          if (it.eq.ileft)  iparen = iparen + 1
          if (it.eq.iright) iparen = iparen - 1
cc          print*, '   ibfr, it, iaft = ', ibfr, it, iaft
cc          print*, (ibfr.ge.jconst), (it.ge.jconst), (iaft.ge.jconst)
c-- check that iparen is never negative (that is left parens
c   before right parens)
          if ((iparen.lt.0).or.((it.eq.ileft).and.(ibfr.gt. 0)).or.
     $         ((it.eq.ileft).and.(iaft.eq.iright)).or.
     $         ((it.eq.iright).and.(iaft.eq.ileft)).or.
     $         ((it.eq.iright).and.(iaft.gt. 0)) ) then
             call echo( synerr//strout(1:ilen) )
             call warn(2, ' parentheses not used properly')
             return
          end if
c-- check that "(," and ",)" are not in string
          if ( (it.eq.icomma).and.((iaft.eq.iright).or.
     $         (ibfr.eq.ileft).or.(ibfr.eq.icomma)) ) then
             call echo( synerr//strout(1:ilen) )
             call warn(2,'  ",,", "(," and ",)" are not allowed')
             return
          end if
c--   check that one-component math operators are followed by "("
          if ( (it.le.-1000).and.(it.ge.-3000) ) then
             if (iaft.ne.ileft) then
                call echo( synerr//strout(1:ilen) )
                call warn(2,' unary math functions must be '//
     $               'followed by "("')
                return
             end if
             if ( (i.gt.1).and.(ibfr.ge.-3000).and.(ibfr.lt.0)
     $            .and.(ibfr.ne.ileft).and.(ibfr.ne.icomma) )  then
                call echo( synerr//strout(1:ilen) )
                call warn(2,' a number is preceded by a '//
     $               'unary math function without using "(" ')
                return
             end if
          end if
c--   look for a real number preceded or followed by a
c     either a real number or a variable
          if ( (it.ge.jscale).and.
     $        ((iaft.ge.jscale).or.(ibfr.ge.jscale)) )then
             call echo( synerr//strout(1:ilen) )
             if (iaft.ge.0) then
                call warn(2,' a real number is followed by '//
     $               'a real number, variable or fixed value')
             else
                call warn(2,' a real number is preceded by '//
     $               'a real number, variable or fixed value')
             endif
             return
          end if
          if ((it.ge.1).and.(iaft.ge.-3000).and.(iaft.le.-1000)) then
             call echo( synerr//strout(1:ilen) )
             call warn(2,'  a number is followed by a'//
     $            ' unary math function')
             return
          end if
c
c  the sepcial functions debye(,) and eins(,), etc, require
c  a certain number of commas (usually 1 or 2)
          if (it.le.-9000)  then
             jsstak = 0
             jcomma = 0
             do  4200 j = i+1, ieqn
                jt   = itemp(j)
                if (jt.eq.iright) then
                   jsstak = jsstak - 1
                   if (jsstak.eq.0) goto 4210
                endif
                if (jt.eq.ileft ) jsstak = jsstak + 1
                if ((jsstak.eq.1).and.(jt.eq.icomma))
     $               jcomma = jcomma + 1
 4200        continue
 4210        continue 
             if (jsstak.ne.0)   jcomma = -jsstak
             emsg = ''
             if ((it.eq.jdebye).and.(jcomma.ne.1)) then
                emsg = ' debye(temp, theta)'
             elseif ((it.eq.jeins).and.(jcomma.ne.1)) then
                emsg = ' eins(temp, theta)'
             elseif ((it.eq.jmin).and.(jcomma.ne.1)) then
                emsg = ' min(x,y)'
             elseif ((it.eq.jmax).and.(jcomma.ne.1)) then
                emsg = ' max(x,y)'

             elseif ((it.eq.jpenl3).and.(jcomma.ne.1)) then
                emsg = ' penalty_hi(x,hi_val)'
             elseif ((it.eq.jpenl2).and.(jcomma.ne.1)) then
                emsg = ' penalty_lo(x,lo_val)'
             elseif ((it.eq.jpenl1).and.(jcomma.ne.2)) then
                emsg = ' penalty(x,lo_val,hi_val)'
             elseif ((it.eq.jlconv).and.(jcomma.ne.2)) then
                emsg = ' lconvolve(x,y,gamma)'
             elseif ((it.eq.jgconv).and.(jcomma.ne.2)) then
                emsg = ' gconvolve(x,y,gamma)'
             elseif ((it.eq.jterpl).and.(jcomma.ne.2)) then
                emsg = ' linterp(xin,yin,xout)'
             elseif ((it.eq.jterpq).and.(jcomma.ne.2)) then
                emsg = ' qinterp(xin,yin,xout)'
             elseif ((it.eq.jterps).and.(jcomma.ne.2)) then
                emsg = ' splint(xin,yin,xout)'
             elseif ((it.eq.jterpa).and.(jcomma.ne.2)) then
                emsg = ' ainterp(xin,yin,xout)'
             elseif ((it.eq.jrebin).and.(jcomma.ne.2)) then
                emsg = ' rebin(xin,yin,xout)'
             elseif ((it.eq.jkktf).and.(jcomma.ne.1)) then
                emsg = ' kkf(e,f1)'
             elseif ((it.eq.jkktr).and.(jcomma.ne.1)) then
                emsg = ' kkr(e,f2)'
             elseif ((it.eq.jrngar).and.(jcomma.ne.2)) then
                emsg = ' range(start,stop,step)'
             elseif ((it.eq.jslica).and.(jcomma.ne.2)) then
                emsg = ' slice(array,istart,istop)'
             elseif ((it.eq.jjoina).and.(jcomma.ne.1)) then
                emsg = ' join(array1,array2)'
             elseif ((it.eq.jnofxa).and.(jcomma.ne.1)) then
                emsg = ' nofx(array,val)'
             elseif ((it.eq.jrngar).and.(jcomma.ne.2)) then
                emsg = ' range(start,stop,step)'
             elseif ((it.eq.jxgaus).and.(jcomma.ne.2)) then
                emsg = ' gauss(x,center,gamma)'
             elseif ((it.eq.jxlore).and.(jcomma.ne.2)) then
                emsg = ' loren(x,center,gamma)'
             elseif ((it.eq.jxstep).and.(jcomma.ne.2)) then
                emsg = ' step(x,center,width)'
             elseif ((it.eq.jxvoit).and.(jcomma.ne.3)) then
                emsg = ' pvoight(x,center,fwhm,eta)'
             elseif ((it.eq.jxcube).and.(jcomma.ne.4)) then
                emsg = ' cube(x,a0,a1,a2,a3)'
             endif
             if (emsg.ne.'') then
                call echo( synerr//strout(1:ilen) )
                call warn(2,'      use   '// emsg)
                return
             end if
          end if
 4400  continue
       ierr = 0
       return
       end
       subroutine engrpn(input,icode)
c
c  convert infix encoded math expression to reverse polish /postfix.
c  works well when part of encod, but has not been well-tested.
c
c strategy:
c    first assign class of operation to each argument in icode.
c    then convert unary minus signs to a one-component operator. 
c    next, two component function are put after their two arguments. 
c    one component operators are then put after their argument.   
c    finally,  parentheses and  commas are dropped.
c
c  copyright (c) 1998  matt newville
c
       include 'encod.h'
       integer   ibfr, istack, maxlen
       parameter(maxlen= 256)
       integer   icode(*),  input(*), itemp(maxlen) , idone(maxlen)
       integer   iclass(maxlen), icltmp(maxlen), idtemp(maxlen)
       integer   iclo(6), i, ic, ichi, icn, id, j, j0, k, ksave
       integer   idebug
       logical   opera
c-- initialize itemp, and assign class to objects and operators
       
c
       idebug = 0
       do 70 j = 1, maxlen
          icode(j) = input(j)
 70    continue 
       do 90 j = 1, maxlen
           i = icode(j)
           if (i.eq.0)                        iclass(j) = 0
           if (i.gt.0)                        iclass(j) = 1
           if ((i.le.-1000).and.(i.ge.-3000)) iclass(j) = 2
           if ((i.eq.iadd) .or.(i.le.isub))   iclass(j) = 3
           if ((i.eq.imul) .or.(i.le.idiv))   iclass(j) = 4
           if (i.eq.iy2x)                     iclass(j) = 5
           if (i.lt.-6000)                    iclass(j) = 6
           if (i.eq.ileft)                    iclass(j) = 7
           if (i.eq.iright)                   iclass(j) = 8
           if (i.eq.icomma)                   iclass(j) = 9
           itemp(j) = icode(j)
           icltmp(j) = iclass(j)
  90    continue
c--
c unary minus and plus --> unitary operators
c minus signs are hard: find next operator at this level,
c and convert  "- x1" to  "neg ( x1 )", which will then be
c converted down below to "x1 neg".
c--
       do 500 j0 = 1, maxlen
          j = j0
 100      continue
          i  = itemp(j)
          ic = icltmp(j)
          if(ic.eq.0) go to 510
          ibfr = 0
          if (j.gt.1)      ibfr = iclass(j-1)
c-- unary plus sign
          if ( ((j.eq.1).or.(ibfr.eq.7).or.(ibfr.eq.4).or.(ibfr.eq.5)
     $         .or.(ibfr.eq.9))    .and.(i.eq.iadd)  )  then
             do 120  k = j, maxlen-1
                icode(k)  = itemp(k+1)
                iclass(k) = icltmp(k+1)
 120         continue
             icode(maxlen)  = 0
             iclass(maxlen) = 0
             do 130  k = j, maxlen
                itemp(k)  = icode(k)
                icltmp(k) = iclass(k)
 130         continue
             go to 100
c-- unary minus sign
c--  change minus sign to unary operator
c--     if next object is (, then we're done.
c--     otherwise, ... - x ... -> ... neg ( x ) ...
          elseif ( ((j.eq.1).or.(ibfr.eq.7).or.(ibfr.eq.9).or.
     $            (ibfr.eq.4).or.(ibfr.eq.5)).and.(i.eq.isub)) then
c  replace '-' with 'neg'
             icode(j)  = ineg
             iclass(j) = 2
c neg number : find next +-,) or end of line, and insert parentheses.
             if (iclass(j+1).eq.1) then
                icn   =  icltmp(j+1)
                opera = (icn.eq.9).or.(icn.eq.8).or.(icn.eq.0)
     $               .or.(icn.eq.3)
c
                if (.not.opera) then
                   istack = 0
                   k      = j
 140               continue
                   k = k + 1
                   if (k.ge.maxlen) go to 150
                   icn   =  icltmp(k)
                   opera =  (icn.eq.9).or.(icn.eq.8).or.(icn.eq.0)
     $                  .or.(icn.eq.3)
                   if ( (istack.eq.0) .and.opera) go to 150
                   if (icn.eq.7) istack = istack + 1
                   if (icn.eq.8) istack = istack - 1
                   go to 140
 150               continue
                   ksave = k -1
c  insert left paren
                   icode(j+1)  = ileft
                   iclass(j+1) = 7
c  bump everything after left paren up by 1
                   do 170  k = j+2, ksave + 1
                      icode(k)  = itemp(k-1)
                      iclass(k) = icltmp(k-1)
 170               continue
c  insert right paren
                   icode(ksave+2)  = iright
                   iclass(ksave+2) = 8
c  bump everything after right paren up by 2
                   do 180  k = ksave+3, maxlen-2
                      icode(k)  = itemp(k-2)
                      iclass(k) = icltmp(k-2)
 180               continue
                end if
c
c neg unary operator : need to find end of argument of operator.
c       then change '-' -> neg and insert parens.
             elseif (((iclass(j+1).eq.2).or.(iclass(j+1).eq.6))
     $               .and.(iclass(j+2).eq.7)) then
c  find end of argument
                istack = 1
                do 200 k = j+3, maxlen
                   if (icltmp(k).eq.7) istack = istack + 1
                   if (icltmp(k).eq.8) istack = istack - 1
                   if (istack.eq.0) go to 220
 200            continue
 220            continue
                ksave = k
c  insert left paren
                icode(j+1)  = ileft
                iclass(j+1) = 7
c  bump everything after left paren up by 1
                do 250  k = j+2, ksave + 1
                   icode(k)  = itemp(k-1)
                   iclass(k) = icltmp(k-1)
 250            continue
c  insert right paren
                icode(ksave+2)  = iright
                iclass(ksave+2) = 8
c  bump everything after right paren up by 2
                do 280  k = ksave+3, maxlen-2
                   icode(k)  = itemp(k-2)
                   iclass(k) = icltmp(k-2)
 280            continue
             end if
             do 380 k = 1, maxlen
                itemp(k) = icode(k)
                icltmp(k) = iclass(k)
 380         continue
             j = j - 1
             if (j.eq.0) j = 1
             go to 100
          end if
c reset itemp and icltmp and go back to beginning
 500   continue
 510   continue
c
       do 600 i = 1, maxlen
          icode(i) = itemp(i)
          iclass(i) = icltmp(i)
 600   continue
c---------------------------------------------------------------------
c-- convert class 5 operators (^ only):
c   x1 ^ x2  -> x1 x2 ^
c    if operator is '^', and is not already followed by ',)+-*/^',
c    then find next place with stack=0 (that is on the current level),
c    that contains an ',)+-*/^'
       ichi = 5
       iclo(1) =  9
       iclo(2) =  8
       iclo(3) =  0
       iclo(4) =  3
       iclo(5) =  4
       iclo(6) =  5
       call pclass(icode, iclass, ichi, iclo)
c---------------------------------------------------------------------
c-- convert class 4 operators (* and / only):
c   x1 * x2  -> x1 x2 *
c    if operator is '*/', and is not already followed by ',)+-*/',
c    then find next place with stack=0 (that is on the current level),
c    that contains an ',)+-*/'
c  undo iclo(6) = '^' to a repeat of iclo(3) = 0
       ichi    =  4
       iclo(6) =  0
       call pclass(icode, iclass, ichi, iclo)
c---------------------------------------------------------------------
c-- convert class 3 operators (+ and - only):
c   x1 + x2  -> x1 x2 +
c    if operator is '+-', and is not already followed by ',)+-',
c    then find next place with stack=0 (that is on the current level),
c    that contains an ',)+-'
c  undo iclo(5) = '*/' to a repeat of iclo(3) = 0
       ichi    =  3
       iclo(5) =  0
       call pclass(icode, iclass, ichi, iclo)
c
       do 900 i = 1, maxlen
          itemp(i) = icode(i)
          icltmp(i) = iclass(i)
 900   continue
c
c---------------------------------------------------------------------
c-- convert class 2 and class 6 operators.
c   all unary operators and special functions have the syntax:
c        f(x1, x2, x3, ...) -> (x1, x2, x3, ...) f
       do 6900 j = 1, maxlen
          idone(j) = 0
          idtemp(j) = 0
 6900  continue
       do 8000 j0 = 1, maxlen - 1
          j  =  j0
 7000     continue
          i  =  itemp(j)
          ic =  icltmp(j)
          id =  idtemp(j)
          if(ic.eq.0) go to 8010
          if ( (id.eq.0).and.
     $         ( (ic.eq.2).or.(ic.eq.6)).and.(iclass(j+1).eq.7)) then
             istack = 1
             do 7200 k = j+2, maxlen
                if (icltmp(k).eq.7) istack = istack + 1
                if (icltmp(k).eq.8) istack = istack - 1
                if (istack.eq.0) go to 7300
 7200        continue
 7300        continue
             ksave = k
             icode(ksave)   = itemp(j)
             iclass(ksave)  = icltmp(j)
             idone(ksave)   = 1
             do 7500  k = j, ksave-1
                icode(k)  = itemp(k+1)
                iclass(k) = icltmp(k+1)
                idone(k)  = idtemp(k+1)
 7500        continue
c     reset itemp and start over again at the same place
             icode(maxlen-1) = 0
             icode(maxlen)   = 0
             idone(maxlen-1) = 0
             idone(maxlen)   = 0
             do 7800 k = 1, maxlen
                itemp(k)  = icode(k)
                icltmp(k) = iclass(k)
                idtemp(k) = idone(k)
 7800        continue
             go to 7000
          end if
 8000  continue
 8010  continue
c---------------------------------------------------------------------
c-- finally, remove all parentheses and commas for icode
cc       print*, 'engrpn: step N-1'
       j = 0
       k = 0
       do 8900 i = 1, maxlen
          itemp(i)  = icode(i)
          icltmp(i) = iclass(i)
          icode(i)  = 0
          iclass(i) = 0
 8900  continue
 9000  continue
       j = j + 1
       if (j.gt.maxlen) go to 9100
       ic = icltmp(j)
       if (ic.eq.0) go to 9100
       if ( (ic.ne.7).and.(ic.ne.8).and.(ic.ne.9)) then
          k = k + 1
          icode(k)  = itemp(j)
          iclass(k) = icltmp(j)
       end if
       go to 9000
 9100  continue
       return
c end subroutine engrpn
       end
       subroutine pclass(icode, iclass, ichi, iclo)
c
c    this is called by engrpn. operators are moved around
c    to convert english math to reverse polish.
c    if operator is of class icin, and is not already followed by
c    an operator with class in iclo,  then find next place with
c    stack=0 (that is on the current level), that contains an
c    operator with class in iclo
c
c  copyright (c) 1998  matt newville
c
       implicit none
       integer i, maxlen, j0, ksave, ic, icn, j, k, istack
       parameter(maxlen = 256)
       integer icode(maxlen), iclass(maxlen), ichi, iclo(6)
       integer itemp(maxlen), icltmp(maxlen)
       logical opera

       do 100 i = 1, maxlen
          itemp(i)  = icode(i)
          icltmp(i) = iclass(i)
 100   continue
       do 2000 j0 = 1, maxlen - 1
          j  =  j0
 500      continue
          ic =  icltmp(j)
cc          print*, '500<< ', j, itemp(j), icltmp(j), ic, ichi, '>>'
          if (ic.eq.0) go to 2010
          if (ic.eq.ichi) then
             icn   =  icltmp(j+1)
             opera = (icn.eq.iclo(1)).or.(icn.eq.iclo(2)).or.
     $            (icn.eq.iclo(3)).or.(icn.eq.iclo(4)).or.
     $            (icn.eq.iclo(5)).or.(icn.eq.iclo(6))
cc             print*, 'opera =', opera
             if (.not.opera) then
                istack = 0
                k      = j
 600            continue
                k = k + 1
                if (k.lt.maxlen) then
                   icn   =  icltmp(k)
                   opera = (icn.eq.iclo(1)).or.(icn.eq.iclo(2)).or.
     $                  (icn.eq.iclo(3)).or.(icn.eq.iclo(4)).or.
     $                  (icn.eq.iclo(5)).or.(icn.eq.iclo(6)) 
cc                   print*, k, icn, ichi, opera
                   if ((istack.ne.0) .or. .not.opera) then
                      if (icn.eq.7) istack = istack + 1
                      if (icn.eq.8) istack = istack - 1
cc                      print*, ' set stack', istack
                      go to 600
                   end if
                end if
                ksave          = k -1
                icode(ksave)   = itemp(j)
                iclass(ksave)  = icltmp(j)
                do 1000  k = j, ksave-1
                   icode(k)  = itemp(k+1)
                   iclass(k) = icltmp(k+1)
 1000           continue
c     reset itemp and start over again at the same place
                icode(maxlen-1) = 0
                icode(maxlen)   = 0
                do 1200 k = 1, maxlen
                   itemp(k) = icode(k)
                   icltmp(k) = iclass(k)
 1200           continue
cc                print*, ' going 500'
                go to 500
             end if
          end if
 2000  continue
 2010  continue
c finish it up
       do 3000 i = 1, maxlen
          icode(i)  = itemp(i)
          iclass(i) = icltmp(i)
 3000  continue
c
       return
c end subroutine class
       end
      subroutine parens(string)
c
c  insert parentheses in a string for a fortran math expression
c  to give the normal math precedence :
c   ^   before   *,/,+,-          and  *,/    before   +,-
c  also:  ** is replaced by ^
c
c  this calls parins, which does the real work of inserting parens.
c
c  copyright (c) 1998  matt newville
c
       character*(*) string, strtmp*2048
       integer       i, ilen,istrln
       external      istrln
c
c  first replace '**' with '^ '
       strtmp = string
       ilen = max(2, istrln(strtmp))
       do 10 i = 1, ilen-1
          if (strtmp(i:i+1).eq.'**')  strtmp(i:i+1) = '^ '
 10    continue
       call unblnk(strtmp)
       ilen = istrln(strtmp)
       if ((strtmp.ne.' ').and.(ilen.gt.0)) then
c
c then put parentheses in to make sure that exponentiation is
c done before multiplication, division, addition, or subtraction.
          if (index(strtmp,'^').ne.0)
     $         call parins(strtmp,ilen,'^','*/+-')
c
c then put parentheses in to make sure that multiplication and
c division are done before addition and subtraction.
          if ((index(strtmp,'*').ne.0).or.(index(strtmp,'/').ne.0))
     $         call parins(strtmp,ilen,'*/','+-')
c
c   put new string into output and return
       endif
       string = strtmp
       return
c end subroutine parens
       end
       subroutine parins(strin, ilen , sopt1, sopt2)
c
c  insert parentheses in a string for a fortran math expression
c  to give the normal math precedence :
c          "sopt1" is more important than "sopt2"
c  this gets kind of ugly but appears to never fail.
c
c  copyright (c) 1998  matt newville
c
       integer     mstack,i, ilen, j, istart, istack, iopt, ioptst
       parameter (mstack = 40)
       character*(*) strin, sopt1, sopt2
       character*2048 string, dummy, str1*1, operas*4, digits*10
       logical       paren(mstack)
       integer       idiff, jstk, i1, i2, io, ieon, nbrstr
       integer       iopen(mstack), istrln
       external      istrln, nbrstr
       data operas, digits / '*/+-', '0123456789'/
c insert a leading blank, initialize stack control and parentheses
       iopt = 0
       dummy = ' '
       dummy(2:ilen+1 ) = strin(1:ilen)
       string = dummy
       istart = 1
       istack = 1
       do 50 i = 1, mstack
          iopen(i)  = 1
          paren(i)  = .false.
 50    continue
 100   continue
       ilen = istrln(string) + 2
       ieon = istart - 1
       do 200 i = istart, ilen
c get current character
c check for exponentiation or parens, update stack index
c and insert parens if they aren't there already
c  note that numbers (found with nbrstr) are skipped over
         str1 = string(i:i)
          if (i.le.ieon) go to 199
          if (index( digits, str1 ).ne.0 ) then
             ieon = i + nbrstr(string(i:))
          elseif (index(sopt1,str1).ne.0) then
             iopt = i
             paren(istack) = .true.
          elseif (str1.eq.'(') then
             istack = istack + 1
             if (istack.gt.mstack) istack = mstack
             iopen(istack) = i
          elseif (str1.eq.')') then
             istack = istack - 1
             if (istack.lt.1) istack = 1
          elseif (index(sopt2, str1 ).ne.0 ) then
             ioptst = i - iopt
             if ( paren(istack)) then
                paren(istack) = .false.
c     normal case: find a far away operation
                if (ioptst.gt.1) then
                   istart = i + 2
                   io = iopen(istack)
                   idiff = i - io
                   if (idiff.gt.1) then
                      dummy = ' '
                      dummy = string(1:io)//'('//
     $                        string(io+1 :i-1)//')'//string(i:)
                      string = dummy
                   end if
c     non-normal case: operation immediately after '^'
                else
                   jstk = 0
                   do 170 j = i + 1, ilen - 2
                      str1 = string(j:j)
                      if (str1.eq.'(') then
                         jstk = jstk + 1
                      elseif (str1.eq.')') then
                         jstk = jstk - 1
                      elseif ( (jstk.eq.0) .and.
     $                        (index(operas,str1).ne.0)) then
                         go to 180
                      end if
 170               continue
 180               continue
                   dummy = ' '
                   dummy = string(:i-1)//'('//string(i: j-1)
     $                  //')'//string(j:)
                   string = dummy
                end if
                go to 100
             else
                iopen(istack) = i
             end if
          end if
 199      continue
 200   continue
c     if needed, insert a last set of parens at the end
       if ( paren(1).and.(iopen(1).ne.1)) then
          i1 = iopen(istack)
          i2 = istrln(string) + 1
          dummy  = ' '
          dummy  = string(1:i1)//'('//
     $         string(i1+1:i2-1)//')'//string(i2:)
          string = dummy
       end if
       call triml(string)
       strin = string
       ilen  = istrln(string)
300    continue
       return
c end soubroutine parins
       end
      integer function nbrstr(string)
c
c  find a number in a string
c  given a string that is known to begin with a digit or sign.
c  return the position of the end of the number.
c  nbrstr : position of end of number
c
c  copyright (c) 1998  matt newville
c
      integer   istrln, i, ilen, iback
      character*(*)  string
      character*1    digit*10, plus, minus, d, e, decml, s, sp
      logical     lexp, ldecml
      data digit  /'1234567890'/
      data minus,plus,d,e,decml /'-','+','d','e','.'/
c------
      ldecml = .false.
      lexp   = .false.
      ilen   = istrln(string)
      nbrstr = ilen
cc      print*, '<nbrstr>{', string(:ilen),'}'
      if (ilen.gt.1) then
         iback  = 1
c find end of number :  digits are always ok.
c stop at second d, e, decml, or sign that's not preceded by (d,e)
         do 200 i = 2, ilen
            sp = string(i-1:i-1)
            s  = string(i:i)
            if (index(digit,s).eq.0) then
               if ( ( (s.ne.plus).and.(s.ne.minus).and.(s.ne.d)
     $                 .and.(s.ne.e).and.(s.ne.decml) )
     $          .or.( lexp.and.((s.eq.d).or.(s.eq.e)) )
     $          .or.( ldecml.and.(s.eq.decml) )
     $          .or.( ( (s.eq.plus).or.(s.eq.minus) ).and.
     $                (sp.ne.d).and.(sp.ne.e) ) )     go to 210
               lexp   = lexp.or.(s.eq.d).or.(s.eq.e)
               ldecml = ldecml.or.(s.eq.decml)
            end if
 200     continue
         iback = 0
 210     continue
         nbrstr = i - 1 - iback
      end if
       return
c  end function nbrstr
       end
       subroutine rpndmp(icode)
c
c purpose: decodes icode array to dump the infix (rpn) description
c          of an encoded formula
c
c copyright (c) 1998  matt newville
       include 'consts.h'
       include 'encod.h'
       include 'arrays.h'
       save
       integer        icode(*), i, ic, ilen, iw, istrln
       character*2048 line, word*32, fword*32
       external       istrln

cc       print*, 'welcome to rpndmp   '
       line = ' '
       ilen = 1
       i    = 0
 10    continue
 20    format (g15.7)
       i    = i + 1
       ic   = icode(i)
       word = ' '
       ic   = icode(i)
       write(word,'(i6)') icode(i)
cc         if (ic.gt.jconst) then
cc            write(word,20) consts(ic - jconst)
cc         elseif ((ic.gt.jscale).and.(ic.le.jconst)) then
cc  cc          print*, i, ic, jscale, ic-jscale
cc  cc          print*, scanam(ic -jscale)
cc            word = scanam(ic - jscale)
cc         elseif (ic.ge.1) then
cc            word = arrnam(ic)
cc         elseif (ic.eq.ileft) then
cc            word = '('
cc         elseif (ic.eq.iright) then
cc            word = ')'
cc         elseif (ic.eq.icomma) then
cc            word = ','
cc         elseif (ic.eq.iexp) then
cc            word = 'exp'
cc         elseif (ic.eq.ilog) then
cc            word = 'ln'
cc         elseif (ic.eq.ilog10) then
cc            word = 'log10'
cc         elseif (ic.eq.isqrt) then
cc            word = 'sqrt'
cc         elseif (ic.eq.isin) then
cc            word = 'sin'
cc         elseif (ic.eq.icos) then
cc            word = 'cos'
cc         elseif (ic.eq.itan) then
cc            word = 'tan'
cc         elseif (ic.eq.iasin) then
cc            word = 'asin'
cc         elseif (ic.eq.iacos) then
cc            word = 'acos'
cc         elseif (ic.eq.iatan) then
cc            word = 'atan'
cc         elseif (ic.eq.iabs) then
cc            word = 'abs'
cc         elseif (ic.eq.ineg) then
cc            word = '-'
cc         elseif (ic.eq.isinh) then
cc            word = 'sinh'
cc         elseif (ic.eq.icosh) then
cc            word = 'cosh'
cc         elseif (ic.eq.itanh) then
cc            word = 'tanh'
cc         elseif (ic.eq.icoth) then
cc            word = 'coth'
cc         elseif (ic.eq.iadd) then
cc            word = '+'
cc         elseif (ic.eq.isub) then
cc            word = '-'
cc         elseif (ic.eq.imul) then
cc            word = '*'
cc         elseif (ic.eq.idiv) then
cc            word = '/'
cc         elseif (ic.eq.iy2x) then
cc            word = '^'
cc         elseif (ic.eq.jadd) then
cc            word = 'add'
cc         elseif (ic.eq.jsub) then
cc            word = 'sub'
cc         elseif (ic.eq.jmin) then
cc            word = 'min'
cc         elseif (ic.eq.jmax) then
cc            word = 'max'
cc         elseif (ic.eq.jkktf) then
cc            word = 'kkf'
cc         elseif (ic.eq.jkktr) then
cc            word = 'kkr'
cc         elseif (ic.eq.jfftf) then
cc            word = 'ftf'
cc         elseif (ic.eq.jfftr) then
cc            word = 'ftr'
cc         elseif (ic.eq.jpenl1) then
cc            word = 'penalty'
cc         elseif (ic.eq.jpenl2) then
cc            word = 'penalty_lo'
cc         elseif (ic.eq.jpenl3) then
cc            word = 'penalty_hi'
cc         elseif (ic.eq.jdebye) then
cc            word = 'debye'
cc         elseif (ic.eq.jeins) then
cc            word = 'eins'
cc         elseif (ic.eq.jgamma) then
cc            word = 'gamma' 
cc         elseif (ic.eq.jlgamm) then
cc            word = 'loggamma' 
cc         elseif (ic.eq.jerf) then
cc            word = 'erf'
cc         elseif (ic.eq.jerfc) then
cc            word = 'erfc'
cc         elseif (ic.eq.jerfcx) then
cc            word = 'erfcx'
cc         elseif (ic.eq.jlconv) then
cc            word = 'lconvolve'
cc         elseif (ic.eq.jgconv) then
cc            word = 'gconvolve'
cc         elseif (ic.eq.jterpl) then
cc            word = 'linterp'
cc         elseif (ic.eq.jterpq) then
cc            word = 'qinterp'
cc         elseif (ic.eq.jterps) then
cc            word = 'splint'
cc         elseif (ic.eq.jterpa) then
cc            word = 'ainterp'
cc         elseif (ic.eq.jrebin) then
cc            word = 'rebin'
cc         elseif (ic.eq.jrngar) then
cc            word = 'range'
cc         elseif (ic.eq.jndarr) then
cc            word = 'indarr'
cc         elseif (ic.eq.j1sarr) then
cc            word = 'ones'
cc         elseif (ic.eq.j0sarr) then
cc            word = 'zeros'
cc         elseif (ic.eq.jasign) then
cc            word = 'sign'
cc         elseif (ic.eq.jceil) then
cc            word = 'ceil'
cc         elseif (ic.eq.jfloor) then
cc            word = 'floor'
cc         elseif (ic.eq.jnpts) then
cc            word = 'npts'
cc         elseif (ic.eq.jvsum) then
cc            word = 'vsum'
cc         elseif (ic.eq.jvprod) then
cc            word = 'vprod'
cc         elseif (ic.eq.jjoina) then
cc            word = 'join'
cc         elseif (ic.eq.jslica) then
cc            word = 'slice'
cc         elseif (ic.eq.jnofxa) then
cc            word = 'nofx'
cc         elseif (ic.eq.-1) then
cc            word = '*variable*'
cc         else 
cc            word = ' '
cc         end if
cc 
cc       fword  = word
       if (ic.ne.0) then
          iw =  istrln(word)
cc          write(fword, '(1x,a,i7,a)')  '(', ic, ') '//word(1:iw)
          write(fword, '(1x,a)')  word(:iw)
          line = line(1:ilen)//' '//fword
       end if
       ilen = max(1,istrln(line))
       if ((i.ge.micode).or.(ic.eq.0).or.(ilen.ge.65)) then
          call triml(line)
          call echo( '     [ '// line(:ilen)//' ]')
          line = ' '
          ilen = 1
          if (i.ge.micode)  call warn(3, ' [ ran out of memory ]')
       endif
       if ((i.lt.micode).and.(ic.ne.0)) go to 10
       
       return
       end
       integer function ienfcn(str)
c convert function name into encod token
       include 'encod.h'
       character*(*) str
       ienfcn = 0

       if (str.eq.'ln') then
          ienfcn = ilog
       elseif (str.eq.'add')  then
          ienfcn = jadd
       elseif (str.eq.'sub')  then
          ienfcn = jsub
       elseif (str.eq.'min')  then
          ienfcn = jmin
       elseif (str.eq.'max')  then
          ienfcn = jmax
       elseif (str.eq.'log')  then
          ienfcn = ilog
       elseif (str.eq.'log10')  then
          ienfcn = ilog10
       elseif (str.eq.'exp')  then
          ienfcn = iexp
       elseif (str.eq.'abs')  then
          ienfcn = iabs
       elseif (str.eq.'sin')  then
          ienfcn = isin
       elseif (str.eq.'cos')  then
          ienfcn = icos
       elseif (str.eq.'tan')  then
          ienfcn = itan
       elseif (str.eq.'npts')  then
          ienfcn = jnpts
       elseif (str.eq.'ceil')  then
          ienfcn = jceil
       elseif (str.eq.'vsum')  then
          ienfcn = jvsum
       elseif (str.eq.'kkf')  then
          ienfcn = jkktf
       elseif (str.eq.'kkr')  then
          ienfcn = jkktr
       elseif (str.eq.'ftf')  then
          ienfcn = jfftf
       elseif (str.eq.'gauss')  then
          ienfcn = jxgaus
       elseif (str.eq.'loren')  then
          ienfcn = jxlore
       elseif (str.eq.'pvoight')  then
          ienfcn = jxvoit
       elseif (str.eq.'cubic')  then
          ienfcn = jxcube
       elseif (str.eq.'step')  then
          ienfcn = jxstep
       elseif (str.eq.'ftr')  then
          ienfcn = jfftr
       elseif (str.eq.'eins')  then
          ienfcn = jeins
       elseif (str.eq.'gamma')  then
          ienfcn = jgamma
       elseif (str.eq.'loggamma') then
          ienfcn = jlgamm
       elseif (str.eq.'erf') then
          ienfcn = jerf
       elseif (str.eq.'erfc') then
          ienfcn = jerfc
       elseif (str.eq.'erfcx') then
          ienfcn = jerfcx
       elseif (str.eq.'sqrt')  then
          ienfcn = isqrt
       elseif (str.eq.'asin')  then
          ienfcn = iasin
       elseif (str.eq.'acos')  then
          ienfcn = iacos
       elseif (str.eq.'atan')  then
          ienfcn = iatan
       elseif (str.eq.'sinh')  then
          ienfcn = isinh
       elseif (str.eq.'cosh')  then
          ienfcn = icosh
       elseif (str.eq.'coth')  then
          ienfcn = icoth
       elseif (str.eq.'tanh')  then
          ienfcn = itanh
       elseif (str.eq.'penalty')  then
          ienfcn = jpenl1
       elseif (str.eq.'penalty_lo')  then
          ienfcn = jpenl2
       elseif (str.eq.'penalty_hi')  then
          ienfcn = jpenl3
       elseif (str.eq.'debye')  then
          ienfcn = jdebye
       elseif (str.eq.'deriv')  then
          ienfcn = jderiv
       elseif (str.eq.'smooth')  then
          ienfcn = jsmoo
       elseif (str.eq.'floor')  then
          ienfcn = jfloor
       elseif (str.eq.'vprod')  then
          ienfcn = jvprod
       elseif (str.eq.'interp')  then
          ienfcn = jterpl
       elseif (str.eq.'lconvolve') then
          ienfcn = jlconv
       elseif (str.eq.'gconvolve') then
          ienfcn = jgconv
       elseif (str.eq.'indarr')  then
          ienfcn = jndarr
       elseif (str.eq.'zeros')  then
          ienfcn = j0sarr
       elseif (str.eq.'range')  then
          ienfcn = jrngar
       elseif (str.eq.'ones')  then
          ienfcn = j1sarr
       elseif (str.eq.'sign')  then
          ienfcn = jasign
       elseif (str.eq.'linterp')  then
          ienfcn = jterpl
       elseif (str.eq.'qinterp')  then
          ienfcn = jterpq
       elseif (str.eq.'ainterp')  then
          ienfcn = jterpa
       elseif (str.eq.'splint')  then
          ienfcn = jterps
       elseif (str.eq.'rebin')  then
          ienfcn = jrebin
       elseif (str.eq.'join')  then
          ienfcn = jjoina
       elseif (str.eq.'slice')  then
          ienfcn = jslica
       elseif (str.eq.'nofx')  then
          ienfcn = jnofxa
       end if
       return
       end
       logical function ic_is_arr(icode, micode)
c decide if an icode returns an array
       include 'encod.h'
       integer j, micode, icode(*)
       ic_is_arr = .false.
       do 10 j = 1,  micode
          ic_is_arr = (((icode(j).gt.0).and.(icode(j).le.jscale))
     $         .or. (icode(j).eq.jndarr)
     $         .or. (icode(j).eq.jrngar)
     $         .or. (icode(j).eq.j1sarr)
     $         .or. (icode(j).eq.jjoina)
     $         .or. (icode(j).eq.j0sarr)  )
          if (ic_is_arr.or.(icode(j).eq.0)) go to 20
 10    continue
 20    continue
       return
       end
       subroutine stack(x,mpts,mstack,npts,istack,ipop)
c
c purpose: part of encod/decod: stack management for decod
c
c arguments:
c      x        stack array  (2d)                    [in/out]
c      mpts     dimension of x (number of points)    [in]
c      mstack   dimension of x (length of stack)     [in]
c      npts     array of number of points for each   [in/out]
c               stack element
c      istack   current length of real stack         [in/out]
c      ipop     number of elements to pop of stack   [in]
c
c notes:
c   1. the stack help in array x is dropped by ipop components
c       ipop = 1 for 2 component math
c
c requires:  none
c
c copyright 1997  matt newville
       integer  mpts, mstack, istack, ipop, i, j
       integer  npts(mstack)
       double precision x(mpts, mstack)
       istack = istack - ipop
       do 30 i = 2, istack
          npts(i) = max(1, min(mpts,npts(i+ipop)))
          do 20 j = 1, npts(i)
             x(j, i)  = x(j, i + ipop)
 20       continue
 30    continue
       do 60 i = istack + 1, istack + ipop
          npts(i) = max(1, min(mpts,npts(i+ipop)))
          do 50 j = 1, npts(i)
             x(j, i)  = 0
 50       continue
 60   continue
       return
c end subroutine stack
       end
       integer function nptstk( n1, n2 )
c   number of vector points for a two-component math operation:
c   n1 = min(n1, n2) unless either n1 or n2 = 1, which
c   means one of the components is a constant, and
c   should be applied to all elements of vector 2
c
c  copyright (c) 1998  matt newville
c
       integer n1, n2
       nptstk = min ( n1, n2 )
       if ( (n1.le.1).or.(n2.le.1) ) nptstk = max ( n1, n2 )
       return
c  end function nptstk
       end
