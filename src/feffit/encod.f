       subroutine encod(string,vnames,nv,consts,nc,icode,ni,ierr)
c
c   copyright 1993  university of washington      matt newville
c
c   this encodes the integer array 'icode' from an equation in the
c   character string 'string'. the companion function *decod* will
c   decode this integer array, returning the proper number.
c   decod is called by:
c             decod(icode, consts, values, defval)
c   the values of 'values' should correspond to the variables named
c   in 'vnames'.  encod and decod are designed for many repeated
c   evaluations. the encoding is slow and slightly redundant and the
c   decoding is as efficient as possible. icode is a small number of
c   integers representing the rpn notation for the math expression,
c   with special integer values specifing all operations and values.
c
c   the character string contains a fortran-like math expression.
c   variables can be used. their names will be held in the character
c   array 'vnames', and their numerical values will be held in the
c   real array 'values'. variables do not need to be explicitly
c   declared before encoding. if a variable is found that has not
c   already been identified, it will be added to the list. the link
c   between variable name and value, and the actual values used are
c   expected to be managed by the routine(s) calling encod and decod.
c
c  input:
c    string  character string containing fortran-like math expression
c    vnames  character array containing variable names
c    nv      dimension of array vnames          (maximum = 8192)
c    consts  real array of numerical constants in math expressions
c    nc      dimension of array consts
c    ni      dimension of icode consts          (maximum = 512)
c  output:
c    string  math expression as to be evaluated (with parens added)
c    vnames  character array containing variable names
c    consts  real array of numerical constants in math expressions
c    icode   integer array containing code for the math expression
c    ierr    error/warning code - routine will not stop !
c           -2     a new variable was added to the list
c           -1     string empty
c            0     no errors or warnings messages at all
c            1     too many constants
c            2     incorrect dimension of vnames
c            3     improper/ambiguous arithmetic
c            5     one-component math syntax error
c            7     parentheses syntax error
c                             ( unmatched or in improper place)
c            9     too many objects in math expresion
c----------------------------------------------------------------------
c    the real array consts contains all the real numbers used as
c    constants. the first 10 values of consts are set aside for
c    "common" real values: 0, 1, 2, pi, etc. these ten constants
c    can be used for accessing internal parameters. the calling
c    routine can associate any values it likes with the first ten
c    numbers, and by rewriting some of this routine, names can be
c    associated with the values. 'pi', and 'reff' are handled in
c    this way. (though it's not expected that anyone will want to
c    overwrite the value of pi, the same is not true for reff).
c
c----------coding parameters for the math operations------------------
c   icode value             meaning
c  -299 to -100     special functions { add(x,y), debye(temp,theta) }
c   -99 to  -50     two-element math operations (x+y, x**y)
c   -49 to  -10     one-element math operations (1/x, sin(x), et c.)
c    -9 to   -6     control operations (open and close parens, comma)
c    -5 or   -1     not possible! (useful for  overwriting/disabling)
c             0     null string
c     1  to jconst  variables corresponding to vnames strings
c jconst to  xxx    constants (numbers in corresponding to consts)
c---------------------------------------------------------------------
c passed variables
       implicit none
       integer       nv, ni, nc, ierr, icode(ni)
       character*(*) string, vnames(nv)
       double precision    consts(nc),   pi, one, zero
       integer    maxlen, jconst, ileft, iright, icomma
       parameter(one = 1.d0, zero = 0.d0, pi = 3.141592653589793d0)
       parameter(maxlen = 1024, jconst = 8192)
       parameter(ileft =  -6, iright =  -7, icomma = -8   )
       integer  iexp, ilog, isqrt, isin, icos, itan, iabs
       integer  iasin, iacos, iatan, isinh, icosh, itanh, icoth
       parameter(iexp  = -10, ilog  = -11, isqrt = -12,
     $             isin  = -13, icos  = -14, itan  = -15,
     $             iasin = -16, iacos = -17, iatan = -18,
     $             iabs  = -19, isinh = -23, icosh = -24,
     $             itanh = -25, icoth = -26  )
       integer     iadd, isub, imul, idiv, iy2x
       parameter(iadd  = -50, isub   = -51, imul   = -52,
     $             idiv  = -53, iy2x   = -54                )
       integer     jadd, jsub, jmin, jmax, jdebye, jeins, jeins2
       parameter(jadd =-111, jsub  =-112, jmin  =-85,
     $      jmax =-86, jdebye=-120, jeins =-121, jeins2=-122)
c
c internal variables
       character*2048  str, strnum, strtmp, errmsg, strout, opera*9
       character*1  str1, straft, strbfr, stri3*3, stri4*4, stri5*5
       character    mtherr*22, synerr*34, encerr*33, number*12
       logical      found, strok
       integer      itemp(maxlen), ntemp, ilen, istr, ieqn
       integer      iparen, iexcla, iperct, ieolc, isave, maxeol
       integer      ibfr, iaft, ibefr, iaftr, ivarln, istrln
       integer      j, jt, jstack, jcomma, ii, it, i, nbrstr
       double precision     xreal
       parameter (number = '1234567890 .' , opera = '+-*/^(), ')
       parameter (mtherr =' math encoding error: ')
       parameter (synerr =' math encoding error: syntax error')
       parameter (encerr =' math encoding error: encod error')
       external     istrln, nbrstr

c-----------------------------------------------------------------------
c  initial error checking of input dimensions
       if (nv.gt.jconst) then
          call echo(mtherr//'incorrect dimension!')
          write (errmsg,'(4x,a,i4,a)') ' more than', jconst,
     $         ' variables are requested '
          ii  = max(1, istrln(errmsg))
          call echo('  '// errmsg(1:ii))
          ierr = 2
          return
       end if
c  initialization
       ierr   = 0
       strtmp = ' '
       str    = ' '
       str1   = ' '
       strnum = ' '
       straft = ' '
       strbfr = ' '
       found  = .false.
c  remove interior blanks from string
       strtmp = string
       ilen   = istrln(strtmp)

       call triml(strtmp)
       call unblnk(strtmp)

       ilen   = istrln(strtmp)
cc       print*, 'UBER #2: ', ilen, ':: ', strtmp(1:ilen)
       maxeol = len(strtmp)
c  remove end-of-line comments:  '!','%'  signify end of line comments
       iexcla = index(strtmp,'!')
       if (iexcla.eq.0) iexcla = maxeol
       iperct = index(strtmp,'%')
       if (iperct.eq.0) iperct = maxeol
       ieolc  = min(iperct,iexcla)
       if (ieolc.eq.1) strtmp = ' '
       if ( (ieolc.ge.2).and.(ieolc.le.maxeol)) then
          str        = strtmp(:ieolc-1)
          str(ieolc:) = ' '
          strtmp     = str
       end if
       ilen   = istrln(strtmp)
cc       print*, 'ENCOD HERE: ', ilen, ':: ', strtmp(1:ilen)
c
c  if string is blank, return
       if ( (strtmp.eq.' ').or.(ilen.le.0).or.(ieolc.eq.1)) then
          icode(1) = 0
          ierr     = -1
          return
       end if
c
c  convert string to the case of this routine :
c    the variable 'case' controls the case of the routine, so it
c    must be the same case as the strings tested for in strtmp.
       call smcase(strtmp, 'c')
       do 40 i = 1, nv
          call smcase(vnames(i), 'c')
 40    continue
c
c  initialized integer arrys to 0
       do 50 i = 1, ni
          icode(i) = 0
 50    continue
       do 60 i=1,maxlen
          itemp(i)  = 0
 60    continue
c
c  set the first values in consts:
c     be careful when changing consts(1) from zero, because the rest
c     of the first ten constants are set to zero also, even though a
c     calling routine may want to overwrite some of them !!!
       consts(1) = zero
       consts(2) = one
       consts(3) = pi
       consts(4) = zero
       consts(5) = zero
       consts(6) = zero
       consts(7) = zero
       consts(8) = zero
       consts(9) = zero
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
       strtmp          = ' '
c-----------------------------------------------------------------------
c  with the string well behaved (parens inserted so there are no
c   ambiguities in the math), let's dechiper it and encode icode
c
c  decipher string into integers
       isave  = 0
       istr   = 0
       ieqn   = 0
       iparen = 0
c  advance string postion, check for end of string
 300   continue
       ieqn = ieqn + 1
 320   continue
       istr = istr + 1
       ibfr = istr - 1
       iaft = istr + 1
       if (ibfr.lt.1) ibfr = 1
       if (istr.gt.ilen) go to 4000
       str1   = string(istr:istr)
       stri3  = string(istr:istr+2)
       stri4  = string(istr:istr+3)
       stri5  = string(istr:istr+4)
       strbfr = string(ibfr:ibfr)
       straft = string(iaft:iaft)
c  ignore blank spaces
       if (str1.eq.' ') go to 320
c-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
c  parse string
c  # constant real number : find length of real number, read it,
c                store it, and advance string to end of number
       if (index(number,str1).ne.0) then
          isave  = istr + nbrstr(string(istr:ilen))
          strnum = string(istr:isave)
          call str2dp(string(istr:isave), xreal, ierr)
          
          if (ierr.ne.0) then
             call echo(mtherr//'cannot read number from string')
             errmsg =  '  >> '//string(istr:isave)//' << '
             call echo(errmsg(1:(isave - istr + 10)))
             ierr = 3
             return
          endif
c       check if constant is already stored. don't start adding more
c       until consts(10), to preserve the stored internal constants
          do 450 i = 1, nc
             if (xreal.eq.consts(i)) then
                itemp(ieqn)  = jconst + i
                go to 500
             elseif ((consts(i).eq.0).and.(i.ge.10)) then
                itemp(ieqn)  = jconst + i
                consts(i) = xreal
                go to 500
             end if
 450      continue
 500      continue
c     error : too many constants!
          if (i.ge.nc) then
             call echo(encerr)
             call echo( ' too many real numbers entered.')
             write (errmsg,'(14x,2a,i4,a)') 'the current',
     $            ' limit is ', nc,' unique numbers.'
             call echo( errmsg )
             ierr = 1
             return
          end if
          istr = isave
c     internally stored constants
c
c     'pi' followed by an operation (or a blank or paren)
c     means use the constant stored in address #(jconst + 3).
       elseif ( (string(istr:istr+1).eq.'pi').and.
     $         (index(opera,string(istr+2:istr+2)).ne.0)  ) then
          itemp(ieqn)  = jconst + 3
          istr         = istr + 1
c
c     'reff' followed by an operation (or a blank or paren)
c     means use the constant stored in address #(jconst + 4).
c             (this is useful for feffit)
       elseif ( (stri4.eq.'reff').and.
     $         (index(opera,string(istr+4:istr+4)).ne.0)  ) then
          itemp(ieqn)  = jconst + 4
          istr         = istr + 3
c
c     'ndegen' followed by an operation (or a blank or paren)
c     means use the constant stored in address #(jconst + 5).
c             (this is useful for feffit)
       elseif ( (string(istr:istr+5).eq.'ndegen').and.
     $         (index(opera,string(istr+6:istr+6)).ne.0)  ) then
          itemp(ieqn)  = jconst + 5
          istr         = istr + 5
c
c      '_k_' followed by an operation (or a blank or paren)
c            means use the constant stored in adress #(jconst + 6).
c            (this is useful for feffit)
c       elseif ( (stri3.eq.'_k_').and.
c     $     (index(opera,string(istr+3:istr+3)).ne.0)  ) then
c             itemp(ieqn)  = jconst + 6
c             istr         = istr + 2
c
c  # end constants
c  # math operations
c   parens and comma
       elseif (str1.eq.'(') then
          itemp(ieqn)  = ileft
          iparen = iparen + 1
       elseif (str1.eq.')') then
          itemp(ieqn)  = iright
          iparen = iparen - 1
       elseif (str1.eq.',') then
          itemp(ieqn)  = icomma
c   two component math
       elseif ((str1.eq.'+').or.(str1.eq.'-')) then
          if ( (straft.eq.')')) then
             call echo(synerr)
             call echo( '     '//strout(1:ilen))
             call echo('  "+)"  and "-)" are not correct syntax.')
             ierr = 3
             return
          else
             if (str1.eq.'+') itemp(ieqn) = iadd
             if (str1.eq.'-') itemp(ieqn) = isub
          end if
       elseif ((str1.eq.'/').or.(str1.eq.'*')) then
          ibefr = index('(+-/*^,',strbfr)
          iaftr = index(')*/^,',straft)
          if ( (istr.eq.1).or.(istr.eq.ilen).or.(iaftr.ne.0)
     $         .or.(ibefr.ne.0)) then
             call echo(synerr)
             call echo( '     '//strout(1:ilen))
             if (ibefr.ne.0) then
                errmsg = '  "/" or "*" preceded by one of "+-/*^,("'
             elseif (iaftr.ne.0) then
                errmsg = '  "/" or "*" followed by one of "/*^,)"'
             elseif (istr.eq.1) then
                errmsg = '  "/" or "*" occurs first'
             elseif (istr.eq.ilen) then
                errmsg = '  "/" or "*" occurs last'
             end if
             ii  = max(1, istrln(errmsg))
             call echo('    '//errmsg(1:ii))
             ierr = 3
             return
          else
             if (str1.eq.'*') itemp(ieqn) = imul
             if (str1.eq.'/') itemp(ieqn) = idiv
          end if
       elseif (str1.eq.'^') then
          ibefr = index('(+-/*^,',strbfr)
          iaftr = index(')*/^,',straft)
          if ( (istr.eq.1).or.(istr.eq.ilen).or.(iaftr.ne.0)
     $         .or.(ibefr.ne.0)) then
             call echo(synerr)
             call echo( '     '//strout(1:ilen))
             if (ibefr.ne.0) then
                errmsg = '  "^" preceded by one of "+-/*^,("'
             elseif (iaftr.ne.0) then
                errmsg = '  "^" followed by one of "/*^,)"'
             elseif (istr.eq.1) then
                errmsg = '  "^" occurs first'
             elseif (istr.eq.ilen) then
                errmsg = '  "^" occurs last'
             end if
             ii  = max(1, istrln(errmsg))
             call echo('    '//errmsg(1:ii))
             ierr = 3
             return
          else
             itemp(ieqn) = iy2x
          end if
c
c   special math functions:
       elseif ((stri4.eq.'add(').or.(stri4.eq.'sub(').or.
     $         (stri4.eq.'min(').or.(stri4.eq.'max(')) then
          if (stri4.eq.'add(')   itemp(ieqn)  = jadd
          if (stri4.eq.'sub(')   itemp(ieqn)  = jsub
          if (stri4.eq.'min(')   itemp(ieqn)  = jmin
          if (stri4.eq.'max(')   itemp(ieqn)  = jmax
          istr   = istr + 2
       elseif (string(istr:istr+5).eq.'debye(') then
          itemp(ieqn)  = jdebye
          istr = istr + 4
       elseif (string(istr:istr+5).eq.'eins2(') then
          itemp(ieqn)  = jeins2
          istr = istr + 4
c
c  one component math :
c    the operator must be followed by '(', or the expression will
c    be a variable name:   ln2 is a variable !
       elseif (stri3.eq.'ln(') then
          itemp(ieqn)  = ilog
          istr = istr + 1
       elseif ((stri4.eq.'log(').or.(stri4.eq.'exp(').or.
     $         (stri4.eq.'abs(').or.(stri4.eq.'sin(').or.
     $         (stri4.eq.'cos(').or.(stri4.eq.'tan(')) then
          if (stri4.eq.'log(')   itemp(ieqn)  = ilog
          if (stri4.eq.'exp(')   itemp(ieqn)  = iexp
          if (stri4.eq.'abs(')   itemp(ieqn)  = iabs
          if (stri4.eq.'sin(')   itemp(ieqn)  = isin
          if (stri4.eq.'cos(')   itemp(ieqn)  = icos
          if (stri4.eq.'tan(')   itemp(ieqn)  = itan
          istr = istr + 2
       elseif ((stri5.eq.'sqrt(').or.(stri5.eq.'asin(').or.
     $         (stri5.eq.'acos(').or.(stri5.eq.'atan(').or.
     $         (stri5.eq.'sinh(').or.(stri5.eq.'cosh(').or.
     $         (stri5.eq.'tanh(').or.(stri5.eq.'coth(').or.
     $         (stri5.eq.'eins(')            ) then
          if (stri5.eq.'sqrt(')  itemp(ieqn)  = isqrt
          if (stri5.eq.'asin(')  itemp(ieqn)  = iasin
          if (stri5.eq.'acos(')  itemp(ieqn)  = iacos
          if (stri5.eq.'atan(')  itemp(ieqn)  = iatan
          if (stri5.eq.'sinh(')  itemp(ieqn)  = isinh
          if (stri5.eq.'cosh(')  itemp(ieqn)  = icosh
          if (stri5.eq.'coth(')  itemp(ieqn)  = icoth
          if (stri5.eq.'tanh(')  itemp(ieqn)  = itanh
          if (stri5.eq.'eins(')  itemp(ieqn)  = jeins
          istr = istr + 3
c# end math operations
c# variables
c          end with blank or math symbol from character string opera
       else
          do 750 i = istr, len(string)
             str1 = string(i:i)
             if (index(opera,str1).ne.0) go to 760
 750      continue
 760      continue
c   find which variable it is:
          ivarln = i - 1
          found = .false.
          if (ivarln.le.istr) ivarln = istr
          do 800 i = 1, nv
             if ( string(istr:ivarln).eq.vnames(i)) then
                found = .true.
                itemp(ieqn)  = i
                istr = ivarln
                go to 810
             end if
 800      continue
 810      continue
c     if it isn't already in vnames, put it in first available slot
          if (.not.found) then
             do 830 i = 1, nv
                if ( vnames(i).eq.' ' ) then
                   vnames(i) = string(istr:ivarln)
                   itemp(ieqn)  = i
                   found = .true.
                   istr  = ivarln
                   go to 840
                end if
 830         continue
 840         continue
          end if
c     if found is still false, then vnames is full.
c     this is then a good time to hurl a warning message.
          if (.not.found) then
             call echo(encerr)
             call echo( ' too many variables declared.')
             write (errmsg,'(14x,2a,i4,a)') 'the current',
     $            ' limit is ', nv,' unique variables.'
             call echo( errmsg )
             ierr = 2
             return
          end if
c
c  # end parsing and encoding, go back to line 300 for more
       end if
       go to 300
c-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
c  some more error checking of string
c-- check that the number of parentheses is correct
 4000  continue
cc       print*, '4000: encod ', str(1:40)
cc       print*, ' code: '
cc       do i = 1, maxlen, 3
cc          if (itemp(i).ne.0) print*,itemp(i),itemp(i+1),itemp(i+2)
cc       end do
       if (iparen.ne.0) then
          call echo(synerr)
          call echo( '     '//strout(1:ilen))
          call echo( '  parentheses are not matched ')
          ierr = 7
          return
       end if
c--
c-- check that one component math functions are followed by "("
c-- and that parentheses are not left hanging
       iparen = 0
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
c-- check that iparen is never negative (that is left parens
c                                        before right parens)
          if (iparen.lt.0) then
             call echo(synerr)
             call echo( '     '//strout(1:ilen))
             call echo( '  parentheses not used properly')
             ierr = 7
             return
          end if
          if ( ( (it.eq.ileft).and.
     $         ( (iaft.eq.iright).or.(ibfr.gt.0) ) )
     $         .or.( (it.eq.iright).and.
     $         ( (iaft.eq.ileft) .or.(iaft.gt.0) ) ) ) then
             call echo(synerr)
             call echo( '     '//strout(1:ilen))
             call echo( '  parentheses not used properly')
             ierr = 7
             return
          end if
c-- check that "(," and ",)" are not in string
          if ( (it.eq.icomma).and.((iaft.eq.iright).or.
     $         (ibfr.eq.ileft).or.(ibfr.eq.icomma))) then
             call echo(synerr)
             call echo( '     '//strout(1:ilen))
             call echo('   ",,", "(," and ",)" are not allowed')
             ierr = 3
             return
          end if
c-- check that one-component math operators are followed by "("
          if ( (it.le.-10).and.(it.ge.-49)) then
             if (iaft.ne.ileft) then
                call echo(synerr)
                call echo( '     '//strout(1:ilen))
                call echo('  unary math functions must be '//
     $               'followed by "("')
                ierr = 5
                return
             end if
             if ( (i.gt.1).and.(ibfr.ge.-49).and.(ibfr.le.-1)
     $            .and.(ibfr.ne.ileft).and.(ibfr.ne.icomma))  then
                call echo(synerr)
                call echo( '     '//strout(1:ilen))
                call echo('  a number is preceded by a '//
     $               'unary math function without using "(" ')
                ierr = 5
                return
             end if
          end if
c--  look for a real number preceded or followed by a
c                    either a real number or a variable
          if ( ((it.ge.300).and.(it.le.600)).and.
     $         ((iaft.ge.1).or.(ibfr.ge.1))) then
             call echo(synerr)
             call echo( '     '//strout(1:ilen))
             if (iaft.ge.0) then
                call echo('  a real number is followed by '//
     $               'a real number, variable or fixed value')
             else
                call echo('  a real number is preceded by '//
     $               'a real number, variable or fixed value')
             end if
             ierr = 3
          end if
          if ((it.ge.1).and.(iaft.ge.-49).and.(iaft.le.-10)) then
             call echo(synerr)
             call echo( '     '//strout(1:ilen))
             call echo('   a number is followed by a'//
     $            ' unary math function')
             ierr = 5
             return
          end if
c
c  the special functions debye(,) and eins(,), etc, require
c  a certain number of commas (usually 1)
          if (it.le.-100)  then
             jstack = 1
             jcomma = 0
cc             print*, ' comma hunt ', i, ieqn, icomma
             do  4200 j = i+2, ieqn
                jt   = itemp(j)
                if (jt.eq.ileft ) jstack = jstack + 1
                if (jt.eq.iright) jstack = jstack - 1
                if ((jstack.eq.1).and.(jt.eq.icomma))
     $               jcomma = jcomma + 1
                strok = .true.
cc                print*, 'STACK:   ' , j, jt, jstack
                if (jstack.eq.0) then
                   strok = .false.
                   if ((it.eq.jdebye).and.(jcomma.ne.1)) then
                      errmsg = ' the function "debye" '//
     $                     'requires 2 arguments and 1 comma'
                      strtmp = ' the proper syntax is: '//
     $                     ' "debye(temp, theta)" '
                   elseif ((it.eq.jeins).and.(jcomma.ne.1)) then
                      errmsg = ' the function "eins" '//
     $                     'requires 2 arguments and 1 comma'
                      strtmp = ' the proper syntax is: '//
     $                     ' "eins(temp, theta)" '
                   elseif ((it.eq.jeins2).and.(jcomma.ne.2)) then
                      errmsg = ' the function "eins2" '//
     $                     'requires 3 arguments and 2 comma'
                      strtmp = ' the proper syntax is: '//
     $                     ' "eins2(temp, theta, mass)" '
                   elseif ((it.eq.jmin).and.(jcomma.ne.1)) then
                      errmsg = ' the function "min" '//
     $                     'requires 2 arguments and 1 comma'
                      strtmp = ' the proper syntax is: '//
     $                     ' "min(x,y)" '
                   elseif ((it.eq.jmax).and.(jcomma.ne.1)) then
                      errmsg = ' the function "max" '//
     $                     'requires 2 arguments and 1 comma'
                      strtmp = ' the proper syntax is: "max(x,y)" '
                   else
                      strok = .true.
                   endif
                   if (.not.strok) then
                      call echo(synerr)
                      call echo( '     '//strout(1:ilen))
                      ii = max(1, istrln(errmsg))
                      call echo('  '// errmsg(1:ii))
                      ii = max(1, istrln(strtmp))
                      call echo('  '// strtmp(1:ii))
                      ierr = 5
                      return
                   end if
                endif
 4200        continue
          end if
 4400  continue
c----------------------------------------------------------------------
c  rewrite itemp to reverse polish notation
c  (this allows easier decoding: see h-p calculator manuals on rpn.)
c  then load up icode, and we're all done.
       j = 0
c        ntemp  = min (ni, maxlen)
c        print*, 'ENCOD 1: ntemp = ', ntemp
c        call rpndmp(itemp,ntemp)

       call engrpn(itemp)
       
c        do i = 1, maxlen
c           if (itemp(i).ne.0) then
c              j = j+1
c              print*, i, j, itemp(i)
c           end if
c        end do
       ntemp  = min (ni, maxlen)
cc       call rpndmp(itemp,ntemp)
       if (itemp(ntemp).ne.0) then
          call echo(mtherr//'too many objects!')
          call echo( '     '//strout(1:ilen))
          write (errmsg,'(4x,a,i4,a)') 'there are more than', ntemp,
     $         ' objects in the math expression for the string: '
          ii  = max(1, istrln(errmsg))
          call echo('  '// errmsg(1:ii))
          call echo('   where objects = operations or numbers')
          call echo('   please break up expression, or contact matt')


          ierr = 9
       end if
       
       do 5000 i = 1, ntemp
          icode(i) = itemp(i)
 5000  continue
       return
c
c end subroutine encod
       end
