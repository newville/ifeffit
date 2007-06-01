       integer function encod(string, jcode, icode)
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
c purpose: converts a math expression into an integer array (icode)
c          for fast evaluation by routine adecod
c
c arguments:
c      string   string for math expression              [in/out]
c      mconst   dimension of array of constants         [in]
c      consts   array of constants                      [in/out]
c      mxicod   dimension of icode      (maximum = 128) [in]
c      jcode    int array of infix   math expression    [out]
c      icode    int array of postfix math expression    [out]
c
c returns
c      encod    error/warning code - see notes          [out]
c
c notes:
c   1. icode will contain a post-fix (rpn) representation of the
c      fortran-like  math expression in string, with special ranges
c      of integers describing different classes of operators and values.
c      see note 6 for a list of integer ranges and meaning.
c   2. encod and adecod are designed for many repeated evaluations.
c      encoding is slow (and slightly redundant) so that decoding is as
c      efficient as possible.
c   3. variables do not need to be explicitly declared before encoding.
c      if a variable is found that has not already been identified, it
c      will be added to the appropriate list (see note 4).  the link
c      between variable names and values should be managed outside.
c   4. array and scalar names are strings that do not begin with
c      a number and do not contain any of '+-*/^(), '.  Case is not
c      important.  Arrays are distinguished from scalars by the present
c       of a '.' in the name:  'A.b' is an array, 'A_b' is a scalar.
c   5. consts contains the real numbers used in the expressions.  no
c      values are internally set  -- they should be set in the
c      calling routine (including the value of pi!!!).
c   6. icode legend: see
c      icode range       meaning
c    -16385: -10  math functions (see encod.h)
c       -9:   -6   control operations (open and close parens, comma)
c       -5:   -1   not possible! (useful for overwriting/disabling/resetting)
c        0         null string
c        1:  8192  arrays:    values in array  array
c     8193: 16384  scalars:   values in scalar array
c    16385:        constants: values in consts array
c
c requires:  encod.h, istrln, triml, echo, lower, unblnk, uncomm,
c            parens, nbrstr, str2dp, enchk, engrpn
c
c parameters
       implicit none
       include 'consts.h'
       include 'encod.h'
       include 'arrays.h'
       save
c passed variables
       character*(*) string
c internal variables
       character*2048 str,  strtmp, strout
       character*256 prefix, errmsg, strnum
       character*1  str1, straft, strbfr, opera*9
       character    str3*3,str4*4, str5*5, str6*6, str7*7, str8*8
       character*32 mtherr, synerr, encerr, number*11, strfcn
       logical   isvect, isvnam, dotrim
       integer   mxicod, ilen, istr, i, isafcn, ienfcn, nxfcn
       integer   itemp(micode), icode(micode), jcode(micode)
       integer   ibfr, iaft, ibefr, iaftr, ivarln, istrln
       integer   iofarr, iofsca, ierr, ieqn, iparen, isave
       integer   ipar, ntest, nscomm, nspow, nssub, nsadd, nsdiv, nsmul
       integer   nsopen, nsclos, nsnum, nbrstr
       double precision    xreal
       external  istrln, isvnam, iofarr, iofsca, ienfcn, nbrstr
       parameter(number = '.0123456789', opera = '+-*/^(), ')
       parameter(mtherr = ' math error: ')
       parameter(synerr = ' math syntax error: ')
       parameter(encerr = ' math encoder out of memory: ')
c
       call gettxt('group', prefix)
c
c  initial error checking of input dimensions
 10    format (i4,a)
c  initialization
       encod  = 0
       strtmp = ' '
       str    = ' '
       str1   = ' '
       strnum = ' '
       straft = ' '
       strbfr = ' '
       strfcn  = ' '
c  remove interior blanks and end-of-line comments:  '!','%','#'
       strtmp = string
       call triml(strtmp)
       call unblnk(strtmp)
       call uncomm(strtmp)
       ilen   = istrln(strtmp)
cc       print*,  'encod 1'
c  if string is blank, return
       icode(1) = 0
       if ( (strtmp.eq.' ').or.(ilen.le.0))  return
cc       print*,  'encod 2:', strtmp(1:ilen), ':', ilen
c
c  convert string to the case of this routine :
c    the variable 'case' controls the case of the routine, so it
c    must be the same case as the strings tested for in strtmp.
       call lower(strtmp)
c
c  initialize integer arrays to 0
       do 50 i = 1, micode
          icode(i) = 0
          jcode(i) = 0
          itemp(i) = 0
 50    continue
c  initialization done.
c-------------------------------------------------------------------
c  now start dealing with strtmp as a math expression
c  fix multiple "unitary" operations: ++, -- -> +; -+, +- -> -
       do 120 i = 1, ilen-1
          str = strtmp(i:i+1)
          if ((str.eq.'--').or.(str.eq.'++')) strtmp(i:i+1) = ' +'
          if ((str.eq.'-+').or.(str.eq.'+-')) strtmp(i:i+1) = ' -'
 120   continue
       call unblnk(strtmp)
c-----------------------------------------------------------------------
c  insert parens to ensure normal math precedence.
c    note that this is not entirely necessary, but it is convenient
c    to rewrite the string in the way it is intended to be evaluated.
       string = strtmp
       call parens(string)
       call triml(string)
       ilen = max(1, istrln(string))
       string(ilen+1:) = ' '
       strout          = string
c
c  translate special cases:
c   note that debye and eins need path index, which is automagically
c    updated from path_index:
       ilen = max(1, istrln(string))
       string(ilen+1:) = ' '
       strtmp          = ' '
cc       print*, ' ENCOD: ', string(1:50)
c-----------------------------------------------------------------------
c  with the string well behaved (parens inserted so there are no
c   ambiguities in the math), let's dechiper it and encode icode
c
c  decipher string into integers
       isave  = 0
       istr   = 0
       ieqn   = 0
       iparen = 0
cc       print*,  'encod 3:', string(1:ilen), ':', ilen
c  advance string postion, check for end of string
 300   continue
       ieqn = ieqn + 1
 320   continue
       istr = istr + 1
       ibfr = istr - 1
       iaft = istr + 1
       if (ibfr.lt.1) ibfr = 1
       if (istr.gt.ilen) go to 4000
cc---------------------------
       strtmp = string(istr:)
       str1   = strtmp(:istr)
c  ignore blank spaces
       if (str1.eq.' ') go to 320

       str3  = strtmp(1:istr+2)
       str4  = strtmp(1:istr+3)
       str5  = strtmp(1:istr+4)
       str6  = strtmp(1:istr+5)
       str7  = strtmp(1:istr+6)
       str8  = strtmp(1:istr+7)
       strbfr = string(ibfr:ibfr)
       straft = string(iaft:iaft)

       nsnum   = index(number,str1)
       nsopen  = index(strtmp(1:),'(')
       nsclos  = index(strtmp(1:),')')
       nsmul   = index(strtmp(1:),'*')
       nsdiv   = index(strtmp(1:),'/')
       nsadd   = index(strtmp(1:),'+')
       nssub   = index(strtmp(1:),'-')
       nspow   = index(strtmp(1:),'^')
       nscomm  = index(strtmp(1:),',')

       if (nsopen.eq.0) nsopen = 999999
       if (nsclos.eq.0) nsclos = 999999
       if (nsmul .eq.0) nsmul  = 999999
       if (nsdiv .eq.0) nsdiv  = 999999
       if (nsadd .eq.0) nsadd  = 999999
       if (nssub .eq.0) nssub  = 999999
       if (nspow .eq.0) nspow  = 999999
       if (nscomm.eq.0) nscomm = 999999
       ntest = min(nsclos,nsmul,nsdiv,nsadd,nssub,nspow,nscomm)
cc       print*, string(1:20),': ',istr,   nsopen, ntest
       isafcn = 0
       if ((nsopen.gt.1).and.(nsopen.lt.ntest)) then
          isafcn = 1
          nxfcn  = nsopen - 1
          strfcn  = strtmp(1:nsopen-1)
cc          print*, "is a function:", strtmp(:nsopen-1),":", nsopen, nxfcn
       end if


c-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
c  parse string
cc       print*, '<encod>', str1
c  # constant real number : find length of real number, read it,
c                store it, and advance string to end of number
       if (index(number,str1).ne.0) then
          isave  = istr + nbrstr(string(istr:ilen))
          strnum = string(istr:isave)
          call str2dp(strnum, xreal, ierr)
          if (ierr.ne.0) then
             call echo(mtherr//'cannot read number from string')
             errmsg = ' << '//string(istr:isave)//' >>'
             call warn(2,errmsg)
             return
          endif
c       check if constant is already stored --
          do 450 i = 1, mconst
             if (xreal.eq.consts(i)) then
                itemp(ieqn)  = jconst + i
                go to 500
             elseif ((consts(i).eq.0).and.(i.gt.1)) then
                itemp(ieqn)  = jconst + i
cc                print*, ' adding constant ', i, xreal
                consts(i) = xreal
                go to 500
             end if
 450      continue
 500      continue
c     error : too many constants!
          if (i.ge.mconst) then
             call warn(3,encerr //' too many constants.')
             return
          end if
          istr = isave
c
c# end constants
c# math operations
c   parens and comma
       elseif (str1.eq.'(') then
          itemp(ieqn)  = ileft
          iparen = iparen + 1
       elseif (str1.eq.')') then
          itemp(ieqn)  = iright
          iparen = iparen - 1
       elseif (str1.eq.',') then
          itemp(ieqn)  = icomma
c two component math
       elseif ((str1.eq.'+').or.(str1.eq.'-')) then
          itemp(ieqn) = iadd
          if (str1.eq.'-') itemp(ieqn) = isub
          if ( (straft.eq.')')) then
             call echo(synerr//strout)
             call warn(2,
     $            '  "+)"  and "-)" are not correct syntax.')
             return
          end if
       elseif ((str1.eq.'/').or.(str1.eq.'*')) then
          itemp(ieqn) = imul
          if (str1.eq.'/') itemp(ieqn) = idiv
          ibefr = index('(+-/*^,',strbfr)
          iaftr = index(')*/^,',straft)

          if ( (istr.eq.1).or.(istr.eq.ilen).or.(iaftr.ne.0)
     $         .or.(ibefr.ne.0)) then
             call echo(synerr//strout)
             if (ibefr.ne.0)  errmsg = 'preceded by one of "+-/*^,("'
             if (iaftr.ne.0)  errmsg = 'followed by one of "/*^,)"'
             if (istr.eq.1)   errmsg = 'occurs first'
             if (istr.eq.ilen) errmsg = 'occurs last'
             call warn(2,'      "/" or "*" '//errmsg)
             return
          end if
       elseif (str1.eq.'^') then
          ibefr = index('(+-/*^,',strbfr)
          iaftr = index(')*/^,',straft)
          itemp(ieqn) = iy2x
          if ( (istr.eq.1).or.(istr.eq.ilen).or.(iaftr.ne.0)
     $         .or.(ibefr.ne.0)) then
             call echo(synerr//strout)
             if (ibefr.ne.0) errmsg = 'preceded by one of "+-/*^,("'
             if (iaftr.ne.0) errmsg = 'followed by one of "/*^,)"'
             if (istr.eq.1)  errmsg = 'occurs first'
             if (istr.eq.ilen) errmsg = 'occurs last'
             call warn(2,'      "^" '//errmsg)
             return
          end if
c
c math functions (one-, and two-component, and special functions)
c    the operator must be followed by '(', or the expression will
c    be a variable name:  ln2 is a variable !
       elseif ((strfcn.ne.' ').and.(isafcn.eq.1)) then
          itemp(ieqn) = ienfcn(strfcn)
          if (itemp(ieqn).eq.0) then
             ilen = istrln(strfcn)
             errmsg = 'unknown function: '//strfcn(1:ilen)//
     $            ' : cannot parse expression'
             call warn(2,errmsg)
             return
          endif
          istr        = istr + nxfcn
          ieqn        = ieqn + 1
          itemp(ieqn) = ileft
          iparen      = iparen + 1
c# end math operations
c# variables
c  end with blank or math symbol from character string opera
       else
          do 750 i = istr, ilen
             str1 = string(i:i)
             if (index(opera,str1).ne.0) go to 760
 750      continue
 760      continue
c find which variable it is:
          ivarln = i - 1
          if (ivarln.le.istr) ivarln = istr
c check if variable name is valid
          if (isvnam(string(istr:ivarln),-1)) then
             isvect = (index(string(istr:ivarln),'.').gt.0)
c if it's array name, find out which array it is
             if (isvect) then
                itemp(ieqn) = iofarr(string(istr:ivarln),prefix,0,1)
             else
                itemp(ieqn) = iofsca(string(istr:ivarln),1) + jscale
             end if
             if (itemp(ieqn).le.0)  then
                call echo(encerr)
                call warn(3,' too many variables declared.')
                return
             end if
             istr = ivarln
          else
             call echo(synerr  //'invalid variable name')
             call warn(2,' --> '// strout(istr:ivarln) )
             return
          endif
c# end parsing and encoding, go back to line 300 for more
       end if
       go to 300
c
 4000  continue
c
c  more error checking :
c  paren count
       if (iparen.ne.0) then
          call echo(synerr//strout(1:ilen))
          call warn(2,' parentheses are not matched ')
          return
       end if

c       call rpndmp(itemp)
c       do i = 1, micode
c          if (itemp(i).ne.0) print*, itemp(i)
c       enddo
       call enchk(strout, itemp, ieqn, ierr)
       encod = ierr
       if (ierr.gt.0) then
          call set_status(2)
          return
       endif

c----
c  rewrite itemp to reverse polish notation
c  then load up icode, and we're all done.
       do 4700 i = 1, micode
          jcode(i) = itemp(i)
 4700  continue 
       call engrpn(jcode, itemp)

       if (itemp(micode).ne.0) then
          call echo(mtherr//'too many objects in math expression!')
          call echo( '     '//strout(1:ilen))
          call warn(3,'   please break up expression.')
       else 
          do 4900 i = 1, micode
             icode(i) =  itemp(i)
 4900     continue
       end if
c prepare output :
c   if we have extra closing parens (and the paren never goes to zero)
c   trim off the outer parens before returning string
       string = strout
       ilen  = istrln(string)
       if ((string(1:1).eq.'(').and.(string(ilen:ilen).eq.')')) then
          ipar = 1
          dotrim = .true.
          do 5300 i = 2, ilen-2
             str1 = string(i:i)
             if (str1.eq.'(')  ipar = ipar + 1
             if (str1.eq.')')  ipar = ipar - 1
             if (ipar.le.0) goto 5350
 5300     continue 
          string = string(2:ilen-1)
 5350     continue 
       endif
c end subroutine encod
       end
