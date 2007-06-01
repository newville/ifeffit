        subroutine engrpn(icode)
c
c  copyright 1993  university of washington      matt newville
c
c      convert english encoded math code to reverse polish code
c
c      this seems to work fairly well when part of encod, but has
c      some difficulty if the input icode is not completely
c      full of parentheses. the code is not extremely well-tested.
c      it is also a bit repetitve.
c
c strategy:
c    first assign class of operation to each argument in icode.
c    then try to convert all unary minus signs to their correct
c    one-component operator.  next, two component function are
c    put after their two arguments. one component operators are
c    then put after thier argument. finally, all parenthese and
c    commas are dropped.
c---------------------------------------------------------------------
       implicit none

       integer   ileft, iright, icomma, ineg, iy2x
       integer   iadd, isub, imul, idiv, maxlen
       parameter(ileft= -6, iright= -7, icomma= -8, ineg= -20,
     $      iy2x= -54, iadd= -50, isub= -51, imul= -52, idiv= -53)
       parameter(maxlen=1024)
       integer   icode(maxlen),  itemp(maxlen) , idone(maxlen)
       integer   iclass(maxlen), icltmp(maxlen), idtemp(maxlen)
       integer   iclo(6), i, ic, ichi, icn, id, j, j0, k, ksave
       integer   ibfr, istack
       logical   opera
c---------------------------------------------------------------------
c-- initialize itemp, and assign class to objects and operators
c
       do 10 j = 1, maxlen
           i = icode(j)
           if (i.eq.0)                      iclass(j) = 0
           if (i.gt.0)                      iclass(j) = 1
           if ((i.le.-10).and.(i.ge.-49))   iclass(j) = 2
           if ((i.eq.iadd) .or.(i.le.isub)) iclass(j) = 3
           if ((i.eq.imul) .or.(i.le.idiv)) iclass(j) = 4
           if (i.eq.iy2x)                   iclass(j) = 5
           if (i.le.-80)                    iclass(j) = 6
           if (i.eq.ileft)                  iclass(j) = 7
           if (i.eq.iright)                 iclass(j) = 8
           if (i.eq.icomma)                 iclass(j) = 9
           itemp(j) = icode(j)
           icltmp(j) = iclass(j)
  10    continue
c---------------------------------------------------------------------
c-- convert unary minus and plus signs to unitary operators
c-- plus signs are easy: simply remove the plus sign.
c-- minus signs are hard: find next operator on this level,
c-- and convert  "- x1" to  "neg ( x1 )", which will then be
c-- converted down below to "x1 neg".
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
       call class(icode, iclass, ichi, iclo)
c---------------------------------------------------------------------
c-- convert class 4 operators (* and / only):
c   x1 * x2  -> x1 x2 *
c    if operator is '*/', and is not already followed by ',)+-*/',
c    then find next place with stack=0 (that is on the current level),
c    that contains an ',)+-*/'
c  undo iclo(6) = '^' to a repeat of iclo(3) = 0
       ichi    =  4
       iclo(6) =  0
       call class(icode, iclass, ichi, iclo)
c---------------------------------------------------------------------
c-- convert class 3 operators (+ and - only):
c   x1 + x2  -> x1 x2 +
c    if operator is '+-', and is not already followed by ',)+-',
c    then find next place with stack=0 (that is on the current level),
c    that contains an ',)+-'
c  undo iclo(5) = '*/' to a repeat of iclo(3) = 0
       ichi    =  3
       iclo(5) =  0
       call class(icode, iclass, ichi, iclo)
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
9100   continue
       return
c end subroutine engrpn
       end
       subroutine parins(strin, ilen , sopt1, sopt2)
c
c  copyright 1993  university of washington      matt newville
c
c  insert parentheses in a string for a fortran math expression
c  to give the normal math precedence :
c          "sopt1" is more important than "sopt2"
c  this gets kind of ugly but appears to never fail.
c--------------------------------------------------------------------
       implicit none

       integer     mstack,i, ilen, j, istart, istack, iopt, ioptst
       parameter ( mstack = 32)
       character*(*) strin, sopt1, sopt2
       character*2048 string, dummy, str1*1, operas*4, digits*10
       logical       paren(mstack)
       integer       idiff, jstk, i1, i2, io, ieon, nbrstr
       integer       iopen(mstack),  istrln
       parameter ( operas  =  '*/+-')
       parameter ( digits  = '0123456789')
       external      istrln, nbrstr
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
      subroutine parens(string)
c
c  copyright 1993  university of washington      matt newville
c  insert parentheses in a string for a fortran math expression
c  to give the normal math precedence :
c   ^   before   *,/,+,-          and  *,/    before   +,-
c  also:  ** is replaced by ^
c
c  this calls parins, which does the real work of inserting parens.
c--------------------------------------------------------------------
       implicit none
       character*(*) string,  strtmp*2048
       integer       i, ilen, istrln
       external      istrln
c
c  first replace '**' with '^ '
       strtmp = string
       ilen = max(2, istrln(strtmp))
       do 10 i = 1, ilen-1
          if (strtmp(i:i+1).eq.'**') then
             strtmp(i:i+1) = '^ '
          end if
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
        subroutine class(icode, iclass, ichi, iclo)
c
c    copyright 1993  university of washington   matt newville
c
c    this is a subroutine of engrpn. operators are moved around
c    to convert english math to reverse polish.
c    if operator is of class icin, and is not already followed by
c    an operator with class in iclo,  then find next place with
c    stack=0 (that is on the current level), that contains an
c    operator with class in iclo
c---------------------------------------------------------------------
        implicit none
       integer i, maxlen, j0, ksave, ic, icn, j, k, istack
       parameter(maxlen =1024)
       integer icode(maxlen),  iclass(maxlen), ichi , iclo(6)
       integer itemp(maxlen),  icltmp(maxlen)
       logical opera
       do 100 i = 1, maxlen
          itemp(i)  = icode(i)
          icltmp(i) = iclass(i)
  100  continue
       do 2000 j0 = 1, maxlen - 1
          j  =  j0
  500     continue
          ic =  icltmp(j)
          if (ic.eq.0) go to 2010
          if (ic.eq.ichi) then
             icn   =  icltmp(j+1)
             opera = .false.
             do 550 i = 1, 6
                if  ( icn.eq.iclo(i)) opera = .true.
 550         continue
             if (.not.opera) then
                istack = 0
                k      = j
 600            continue
                   k = k + 1
                   if (k.ge.maxlen) go to 700
                   icn   =  icltmp(k)
                   opera = .false.
                   do 650 i = 1, 6
                      if  ( icn.eq.iclo(i)) opera = .true.
 650               continue
                   if ( (istack.eq.0) .and. opera) go to 700
                    if (icn.eq.7) istack = istack + 1
                   if (icn.eq.8) istack = istack - 1
                   go to 600
 700            continue
                ksave = k -1
                icode(ksave)   = itemp(j)
                iclass(ksave)  = icltmp(j)
                do 1000  k = j, ksave-1
                   icode(k)  = itemp(k+1)
                   iclass(k) = icltmp(k+1)
1000            continue
c     reset itemp and start over again at the same place
                icode(maxlen-1) = 0
                icode(maxlen)   = 0
                do 1200 k = 1, maxlen
                   itemp(k) = icode(k)
                   icltmp(k) = iclass(k)
1200            continue
                go to 500
             end if
          end if
2000   continue
2010   continue
c finish it up
       do 3000 i = 1, maxlen
          icode(i) = itemp(i)
          iclass(i) = icltmp(i)
3000   continue
c
      return
c end subroutine class
      end
