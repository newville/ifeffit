       double precision function decod(icode, ni,
     $      consts, values, defval)
c
c  copyright 1993 .. 1997 matt newville
c
c   this decodes the icode array, inserting the values in consts,
c   and values when necessary, and returns the real value of the
c   calulated pararmeter.
c   the default value, defval, will be returned if icode is empty.
c
c  input:
c    icode    integer array containing code for the math expression
c    ni       length of icode
c    consts   real array of the constant numerical values
c    values   real array containing the variable values
c    defval   default value for the parameter
c  output:
c    decod    real number calculated from integer code in icode
c---------------------------------------------------------------------
       integer  mstack, ierr, iplace, istack, ic, i, ni
       double precision f1mth, f2mth, zero, cordby, einsdw, einval
       integer  jconst, icode(ni)
       parameter(mstack=  32, jconst = 8192, zero = 0.)
       double precision  consts(*), values(*), x(mstack), defval
       integer  iexp, ilog, isqrt, isin, icos, itan, iabs, ineg
       integer  iasin, iacos, iatan, isinh, icosh, itanh, icoth
       integer  iadd, isub, imul, idiv, iy2x
       integer  jadd, jsub, jmin, jmax, jdebye, jeins, jeins2
       parameter(iexp  = -10, ilog  = -11, isqrt = -12,
     $           isin  = -13, icos  = -14, itan  = -15,
     $           iasin = -16, iacos = -17, iatan = -18,
     $           iabs  = -19, ineg  = -20, isinh = -23,
     $           icosh = -24, itanh = -25, icoth = -26,
     $           iadd  = -50, isub  = -51, imul  = -52,
     $           idiv  = -53, iy2x  = -54, jadd  =-111,
     $           jsub  =-112, jmin  = -85, jmax  = -86,
     $           jdebye=-120, jeins =-121, jeins2=-122)
       external  f1mth, f2mth, cordby, einsdw, einval
c
c  return default value if icode is empty
       decod = defval
       if (icode(1).eq.0)  return
c  initialize stack
       do 20 i = 1, mstack
          x(i) = zero
 20    continue
       ierr   = 0
       iplace = 0
       istack = 0
c  interpret next object, do operations, and manage stack
c                hold place in icode array with iplace
 100   continue
       iplace = iplace + 1
       if (iplace.gt.ni) ierr = 2
       ic = icode(iplace)
       if (ic.eq.0) return
c  if number, then push everything in stack
       if (ic.ge.1) then
          istack = istack + 1
          if (istack.ge.mstack) ierr = 1
          do 200 i = istack, 2, -1
             x(i) = x(i-1)
 200      continue
          if (ic.le.jconst) x(1) = values(ic)
          if (ic.gt.jconst) x(1) = consts(ic - jconst)
c  one-component math:  overwrite x(1), no change to rest of the stack
       elseif ( (ic.le.-10).and.(ic.ge.-49)) then
          x(1) = f1mth(x(1),ic,ierr)
c  two-component math:  overwrite x(1), drop x(2), drop stack by 1
       elseif ( (ic.le.-50).and.(ic.ge.-99)) then
          x(1) =  f2mth( x(1),x(2),ic,ierr)
          call stack(x,istack,1)
c  special math operations:
c    each has its own external function call, and stack size.
c      : debye( x(2), x(1))
       elseif (ic.eq.jdebye) then
          x(1) =  cordby(x(2), x(1), ierr)
          call stack(x,istack,1)
c      : eins( x(2), x(1))
       elseif (ic.eq.jeins) then
          x(1) =  einsdw(x(2), x(1), ierr)
          call stack(x,istack,1)
c      : eins2( x(3), x(2), x(1))
       elseif (ic.eq.jeins2) then
          x(1) =  einval(x(3), x(2), x(1))
          call stack(x,istack,2)
       else
          ierr = 3
       end if
c  done: if there were no errors, update decod and go to next object
       if (ierr.eq.0) then
          decod = x(1)
          go to 100
       end if
c  error handling: can only get here if (ierr.ne.0)
       decod  = zero
       if (ierr.eq.ilog) then
          call echo(' math error: log(x) must have x > 0')
       elseif (ierr.eq.isqrt) then
          call echo(' math error: sqrt(x) cannot have x < 0')
       elseif (ierr.eq.iasin) then
          call echo(' math error: asin(x) must have (-1 < x < 1)')
       elseif (ierr.eq.iacos) then
          call echo(' math error: acos(x) must have (-1 < x < 1)')
       elseif (ierr.eq.idiv) then
          call echo(' math error: divide by 0')
       elseif (ierr.eq.iy2x) then
          call echo(' math error: invalid exponentiation')
       elseif (ierr.eq.jdebye) then
          call echo(' math error: error using "debye" function')
          call echo('      could not find a path index to use!')
       elseif (ierr.eq.jeins) then
          call echo(' math error: error using "eins" function')
          call echo('      could not find a path index to use!')
       elseif (ierr.eq.1) then
          call echo(' decoding error: exceeded stack size!')
       elseif (ierr.eq.2) then
          call echo(' decoding error: too many objects!')
       elseif (ierr.eq.3) then
          call echo(' decoding error: unknown operation!')
       else
          call echo(' decoding error: unknown error!')
       end if
       if (ierr.gt.0) call fstop(' decoding error')
       return
c  end function decod
       end
       double precision function f1mth( x, iop, ierr)
c
c  copyright 1993  university of washington      matt newville
c
c one component math. if any tests are failed, x is returned.
c iop is an integer indication of which operation to perform.
c
       double precision   x, zero, one, expmax
       logical     error
       integer     iop, ierr
       integer     iexp, ilog, isqrt, isin, icos, itan, iabs, ineg
       integer     iasin, iacos, iatan, isinh, icosh, itanh, icoth
       parameter ( iexp  = -10, ilog  = -11, isqrt = -12,
     $             isin  = -13, icos  = -14, itan  = -15,
     $             iasin = -16, iacos = -17, iatan = -18,
     $             iabs  = -19, ineg  = -20, isinh = -23,
     $             icosh = -24, itanh = -25, icoth = -26  )
       parameter (zero = 0.d0, one = 1.d0, expmax = 50.d0)
       ierr  = 0
       error = .false.
       f1mth = zero
       if (iop.eq.iexp) then
          f1mth = exp( max( -expmax, min(x, expmax) ) )
       elseif (iop.eq.ilog) then
          if  ( x.gt.zero ) then
             f1mth = log(x)
          else
             error = .true.
          end if
       elseif (iop.eq.isqrt) then
          if  ( x.ge.zero) then
             f1mth = sqrt(x)
          else
             error = .true.
          end if
       elseif (iop.eq.iabs)  then
          f1mth = abs(x)
       elseif (iop.eq.ineg)  then
          f1mth = - x
       elseif (iop.eq.isin)  then
          f1mth = sin(x)
       elseif (iop.eq.icos)  then
          f1mth = cos(x)
       elseif (iop.eq.itan)  then
          f1mth = tan(x)
cc       elseif (iop.eq.icot)  then
cc          f1mth = one / max( small, tan(x) )
       elseif (iop.eq.iasin) then
          if (dabs(x).le.one) then
             f1mth = asin(x)
          else
             error = .true.
          end if
       elseif (iop.eq.iacos) then
          if (dabs(x).le.one) then
             f1mth = acos(x)
          else
             error = .true.
          end if
       elseif (iop.eq.iatan) then
          f1mth = atan(x)
       elseif (iop.eq.itanh) then
          f1mth = tanh(  max(-expmax, min(x, expmax)) )
       elseif (iop.eq.icoth) then
          f1mth = one / tanh(  max(-expmax, min(x, expmax)) )
       elseif (iop.eq.icosh) then
          f1mth = cosh(  max(-expmax, min(x, expmax)) )
       elseif (iop.eq.isinh) then
          f1mth = sinh(  max(-expmax, min(x, expmax)) )
       else
          f1mth = x
       end if
       if (error) ierr = iop
       return
c end function f1mth
       end
       double precision  function f2mth( x, y, iop, ierr)
c
c  copyright 1993  university of washington      matt newville
c
c two component math.
c if ( (negative number)**(fraction)) is requested, x is returned.
c
c iop is an integer indication of which operation to perform.
c if iop = 0, then x is returned.
       double precision x, y, zero, one, fifty, xtmp, test
       integer   ierr, iadd, isub, imul, idiv, iy2x, iop, newx
       integer   jmin, jmax
       parameter ( iadd = -50, isub = -51, imul  = -52)
       parameter ( idiv = -53, iy2x  = -54 )
       parameter ( jmin  = -85, jmax  = -86)
       parameter ( zero = 0.d0, one = 1.d0, fifty = 50.d0)
       ierr  = 0
       f2mth = zero
       if (iop.eq.0) then
          f2mth = x
       elseif (iop.eq.iadd) then
          f2mth = y + x
       elseif (iop.eq.isub) then
          f2mth = y - x
       elseif (iop.eq.imul) then
          f2mth = y * x
       elseif (iop.eq.jmin) then
          f2mth = min(y, x)
       elseif (iop.eq.jmax) then
          f2mth = max(y, x)
       elseif (iop.eq.idiv) then
          if (x.eq.zero)  then
             f2mth = zero
             ierr  = iop
          else
             f2mth = y / x
          end if
       elseif (iop.eq.iy2x) then
          newx = int(x)
          xtmp = float(newx)
          if (x.eq.zero)  then
             f2mth = one
          elseif ( (y.eq.zero).and.(x.gt.zero))  then
             f2mth = zero
          elseif (y.gt.zero)  then
             test  = x * log(y)
             if (test.gt.fifty) then
                f2mth = exp(fifty)
             elseif (test.lt.(-fifty)) then
                f2mth = exp(-fifty)
             else
                f2mth = y**x
             end if
          elseif ( (y.lt.zero).and.(xtmp.eq.x)) then
             test  = xtmp * log(-y)
             if (test.gt.fifty) then
                f2mth = exp(fifty)
             elseif (test.lt.(-fifty)) then
                f2mth = exp(-fifty)
             else
                f2mth = y**newx
             end if
          else
             f2mth = zero
             ierr  = iop
          end if
       end if
       return
c end function f2mth
       end
       subroutine stack(xstack,istack,ipop)
c
c   copyright 1993  university of washington      matt newville
c   drop the stack held in x by idrop. (ipop = 1 for 2 component math)
       double precision   xstack(*)
       integer istack, ipop, i
       istack   = istack - ipop
       do 100 i = 2, istack
          xstack(i)  = xstack(i + ipop)
 100   continue
       do 120 i = istack + 1, istack + ipop
          xstack(i)  = 0.d0
 120   continue
       return
c  end subroutine stack
       end
