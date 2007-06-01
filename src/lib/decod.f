       subroutine decod(icode, micode, consts, scalar, array, 
     $       narray, nparr, mvpts, maxarr, nvout, outval)
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
c purpose: decodes icode array (created by encod), returning real vector
c          outval, containing results of performing the math expression
c          described by the icode array.
c
c arguments:
c      icode    integer array -- coded math expression  [in]
c      micode   dimension of icode                      [in]
c      consts   array of constants                      [in]
c      scalar   array of scalar values                  [in]
c      arrays   array of vector values (mvpts,maxarr)   [in]
c      narray    integer array : npts for each array     [in]
c      mvpts    max # of array elements in arrays       [in]
c      maxarr   max # of arrays                         [in]
c      nvout    number of points in outval          [in/out]
c      outval   array of output values  (mvpts)     [in/out]
c               default values on input
c notes:
c   1. mvpts cannot exceed maxpts
c   2. this decodes the icode array, inserting constants,scalars,arrays,
c      and performing math operations, to return the calulation to outval.
c   3. outval will be returned if icode is empty.
c   4. see also encod
c
c requires:  encod.h, f1mth, f2mth, v1mth, stack, echo, eins
c
       implicit none
       include 'maxpts.h'
       include 'encod.h'
       integer    micode, mvpts, maxarr, nvout, mstack
       parameter(mstack=12)
       integer  icode(micode), narray(maxarr), nparr(maxarr)
       double precision  consts(*), scalar(*), array(*)
       double precision  outval(mvpts), x(maxpts, mstack)
       double precision  xtmp(maxpts) , xs_tmp, xx0, dx, tiny, small
       parameter   (tiny = 1.d-9, small = 1.d-4)
       integer  nx(mstack), ierr, istack, ic, i, j, iplace, iarx, npx
       integer  nofxa, itmp1, itmp2
       character mtherr*14, death*21
       parameter(mtherr = ' * math error ')
       parameter(death  = ' * math parser died: ')
       external nofxa
c
c-- return default if the icode is empty
cc       print*, ' decod: ', icode(1), icode(2), icode(3)
       ierr   = 0
       iplace = 0
       istack = 0
       if (mvpts.gt.maxpts) then
          ierr = 5
          go to 1000
       endif
       if (nvout.le.0) nvout = 1
       if ((icode(1).eq.0) .or. (icode(1).eq.-1))  return
c-- initialize stack
       do 50 i = 1, mstack
          nx(i)  = 0
          x(1,i) = 0
          x(2,i) = 0
 50    continue
c--    interpret next object, do operations, and manage stack
c      hold place in icode array with iplace

 100   continue
       iplace = iplace + 1
       if (iplace.gt.micode)   ierr = 2
       ic = icode(iplace)
       if ((ic.eq.0).or.(ic.eq.-1)) then
          nvout = nx(1)
          do 104 i = 1, nx(1)
             outval(i) = x(i,1)
 104      continue
          return
c if number, then push everything in stack and fill in the first element
       elseif (ic.ge.1) then
c   push stack:
          istack = istack + 1
          if (istack.ge.mstack)  ierr = 1
          do 150 i = istack, 2, -1
             nx(i) = nx(i-1)
             do 140 j = 1, nx(i)
                x(j,i)  = x(j,i-1)
 140         continue
 150      continue 
c   fill with constant
          if (ic.gt.jconst) then
             x(1,1)  = consts(ic - jconst)
             nx(1)   = 1
c   fill with scalar
          elseif ((ic.gt.jscale).and.(ic.le.jconst)) then
             x(1,1)  = scalar(ic - jscale)
             nx(1)   = 1
c   fill with array
          else 
             nx(1)   = max(1, min(maxpts, narray(ic)))
             iarx = ic
             if (iarx.eq.0) then
                do 248 j = 1, nx(1)
                   x(j,1)  = 0
 248            continue
             else
cc                print*, 'DECOD iarr = ', iarx, nparr(iarx), narray(iarx)
cc                print*, 'DECOD arr:',array(nparr(iarx)),
cc     $               array(nparr(iarx)+1)
                npx = nparr(iarx)-1
                do 250 j = 1, nx(1)
                   x(j,1)  = array(npx+j)
 250            continue
             endif
          endif
c  1-component math: overwrite x(1)
c  iop range: -128 to -4096
       elseif ( (ic.le.-1000).and.(ic.ge.-3000) ) then
          call f1mth(x(1,1),nx(1),ic,ierr)
c  2-component math: overwrite x(1), drop x(2), pop stack, update nx(1)
c  iop range: -5000 to -6000
       elseif ( (ic.le.-5000).and.(ic.ge.-8000) ) then
          call f2mth( x(1,1), nx(1), x(1,2), nx(2), ic, ierr)
          call stack(x,maxpts,mstack,nx,istack,1)
c  1-component vector operations: floor, ceil value of an array,
c         sum or product of all elements in a single array
c  iop range: -30000 to -32000
       elseif ((ic.le.-30000).and.(ic.ge.-32000)) then
          call v1mth(x(1,1),nx(1),ic,ierr)
c  special math operations:
c  iop range: -8000 to -10000
       elseif (ic.eq.jdebye) then
          call cordby(x(1,1),nx(1),x(1,2),nx(2),ierr)
          call stack(x,maxpts,mstack,nx,istack,1)
       elseif (ic.eq.jeins) then
          call eins(x(1,1),nx(1),x(1,2),nx(2),ierr)
          call stack(x,maxpts,mstack,nx,istack,1)
       elseif (ic.eq.jkktf) then
          call w_kkf(x(1,1),nx(1),x(1,2),nx(2),ierr)
          call stack(x,maxpts,mstack,nx,istack,1)
       elseif (ic.eq.jkktr) then
          call w_kkr(x(1,1),nx(1),x(1,2),nx(2),ierr)
          call stack(x,maxpts,mstack,nx,istack,1)
       elseif (ic.eq.jfftf) then
          call w_fftf(x(1,1),nx(1),ierr)
       elseif (ic.eq.jfftr) then
          call w_fftr(x(1,1),nx(1),ierr)

       elseif (ic.eq.jpenl1) then
          call res_penalty(x(1,1),nx(1),x(1,2),nx(2),x(1,3),nx(3))
          call stack(x,maxpts,mstack,nx,istack,2)

       elseif (ic.eq.jpenl2) then
          xx0 = - exp(85.d0)
          call res_penalty(x(1,1),nx(1),xx0,1,x(1,2),nx(2))
          call stack(x,maxpts,mstack,nx,istack,1)

       elseif (ic.eq.jpenl3) then
          xx0 = exp(85.d0)
          call res_penalty(xx0, 1, x(1,1),nx(1),x(1,2),nx(2))
          x(1,1) = xx0
          call stack(x,maxpts,mstack,nx,istack,1)

       elseif ((ic.eq.jterpl).or.(ic.eq.jterpq).or.
     $         (ic.eq.jterps).or.(ic.eq.jterpa) ) then
          j = jterpl - ic
          call xterp(x(1,1),nx(1),x(1,2),nx(2),x(1,3),nx(3),j,ierr)
          call stack(x,maxpts,mstack,nx,istack,2)
       elseif (ic.eq.jrebin) then
          call rebin_interp(x(1,1),nx(1),x(1,2),nx(2),x(1,3),nx(3))
          call stack(x,maxpts,mstack,nx,istack,2)
       elseif (ic.eq.jlconv) then
c                       gamma,       x        y   yout
cc          print*, ' convolve: gamma = ', x(1,1), nx(2), nx(3)
cc          print*, ' convolve: x     = ', x(1,3), x(2,3), x(3,3)
cc          print*, ' convolve: y     = ', x(1,2), x(2,2), x(3,2)
          xx0 = 0.d0
          call conv_lor(x(1,1),nx(2),x(1,3),x(1,2),xx0,xtmp)
          nx(1) = nx(2)
          do 320 j = 1, nx(1)
             x(j,1)  = xtmp(j)
 320      continue
cc          print*, 'done'
          call stack(x,maxpts,mstack,nx,istack,2)
       elseif (ic.eq.jgconv) then
c                       gamma,       x        y   yout
cc          print*, ' convolve: gamma = ', x(1,1), nx(2), nx(3)
cc          print*, ' convolve: x     = ', x(1,3), x(2,3), x(3,3)
cc          print*, ' convolve: y     = ', x(1,2), x(2,2), x(3,2)
          xx0 = 0.d0
          call conv_gau(x(1,1),nx(2),x(1,3),x(1,2),xx0,xtmp)
          nx(1) = nx(2)
          do 370 j = 1, nx(1)
             x(j,1)  = xtmp(j)
 370      continue
          call stack(x,maxpts,mstack,nx,istack,2)
c range:   start stop step
       elseif (ic.eq.jrngar) then
          xx0   = x(1,3)
          dx    = x(1,1)
          if (abs(dx) .le.tiny) dx = tiny
          nx(1) = int((abs(x(1,2)-x(1,3)) + small*abs(dx))/ abs(dx))+1
          nx(1) = max(1, min(maxpts, nx(1)))
          do 390 j = 1, nx(1)
             x(j,1)  = xx0 + (j-1)*dx
 390      continue
          call stack(x,maxpts,mstack,nx,istack,2)
       elseif (ic.eq.jndarr) then
          nx(1) = max(1, min(maxpts, int(x(1,1))))
          do 420 j = 1, nx(1)
             x(j,1)  = j
 420      continue
       elseif (ic.eq.j1sarr) then
          nx(1) = max(1, min(maxpts, int(x(1,1))))
          do 440 j = 1, nx(1)
             x(j,1)  = 1
 440      continue
       elseif (ic.eq.j0sarr) then
          nx(1) = max(1, min(maxpts, int(x(1,1))))
          do 450 j = 1, nx(1)
             x(j,1)  = 0
 450      continue
       elseif (ic.eq.jxgaus) then
          call do_gauss(x(1,3),nx(3),x(1,2),x(1,1),xtmp)
          nx(1) = nx(3)
          do 502 j = 1, nx(1)
             x(j,1)  = xtmp(j)
 502      continue
          call stack(x,maxpts,mstack,nx,istack,2)
       elseif (ic.eq.jxlore) then
          call do_loren(x(1,3),nx(3),x(1,2),x(1,1),xtmp)
          nx(1) = nx(3)
          do 505 j = 1, nx(1)
             x(j,1)  = xtmp(j)
 505      continue
          call stack(x,maxpts,mstack,nx,istack,2)
       elseif (ic.eq.jxvoit) then
          call do_pvoight(x(1,4),nx(4),x(1,3),x(1,2),x(1,1),xtmp)
          nx(1) = nx(4)
          do 510 j = 1, nx(1)
             x(j,1)  = xtmp(j)
 510      continue
          call stack(x,maxpts,mstack,nx,istack,3)
       elseif (ic.eq.jnofxa) then
cc          print*, ' nofx !'
          j = nofxa(x(1,1),x(1,2),nx(2))
          nx(1) = 1 
          x(1,1) = j
          call stack(x,maxpts,mstack,nx,istack,1)
       elseif (ic.eq.jjoina) then
          j = nx(2)
          itmp1 = min(nx(1)+nx(2), maxpts)-nx(2)
          do 515 i = 1,nx(2)
             xtmp(i) = x(i,2)
 515      continue 
          do 516 i  = 1,itmp1
             xtmp(j+i) = x(i,1)
 516      continue 
          nx(1) = max(1,itmp1+nx(2))
          do 517 i  = 1,nx(1)
             x(i,1) = xtmp(i) 
 517      continue 
          call stack(x,maxpts,mstack,nx,istack,1)
       elseif (ic.eq.jslica) then
          itmp1 = min(int(x(1,1)),nx(3))
          itmp2 = max(int(x(1,2)),1)
          nx(1) = itmp1-itmp2+1
          do 530 i  = 1, nx(1)
             xtmp(i) = x(i+itmp2-1,3)
 530      continue
          do 532 i  = 1, nx(1)
             x(i,1) = xtmp(i)
 532      continue
          call stack(x,maxpts,mstack,nx,istack,2)
       else
          ierr = 9999
       end if
c  done : if no errors, then update param and go to next object
       if (ierr.eq.0) go to 100
c error handling
 1000  continue 
       if (ierr.eq.ilog) then
          call warn(2,mtherr//'-- log(x) needs  x > 0')
       elseif (ierr.eq.ilog10) then
          call warn(2,mtherr//'-- log10(x) needs  x > 0')
       elseif (ierr.eq.isqrt) then
          call warn(2,mtherr//'-- sqrt(x) needs x >= 0')
       elseif (ierr.eq.iasin) then
          call warn(2,mtherr//'-- asin(x) needs (-1< x <1)')
       elseif (ierr.eq.iacos) then
          call warn(2,mtherr//'-- acos(x) needs (-1< x <1)')
       elseif (ierr.eq.idiv) then
          call warn(2,mtherr//'-- divide by 0 ')
       elseif (ierr.eq.iy2x) then
          call warn(2,mtherr//'-- invalid exponentiation ')
       elseif (ierr.eq.jdebye) then
          call warn(2,mtherr//' using "debye" function ')
          call warn(2,' *     could not find a path index to use!')
       elseif (ierr.eq.jeins) then
          call warn(2,mtherr//' using "eins" function')
          call warn(2,' *     could not find a path index to use!')
       elseif (ierr.eq.1) then
          call warn(2,death//'exceeded stack size')
       elseif (ierr.eq.2) then
          call warn(2,death//'too many objects')
       elseif (ierr.eq.5) then
          call warn(2,death//'mvpts .gt. maxpts')
       elseif (ierr.eq.9999) then
          call warn(2,death//' unknown operation')
       else
          call warn(2,death//' i am truly confused')
       end if
       if (ierr.gt.0) then
          call set_status(3)
       endif
c end subroutine decod
       end
       subroutine f1mth( x, nx, iop, ierr)
c
c one component math. if any tests are failed, x is returned.
c iop is an integer indication of which operation to perform.
c
c  copyright (c) 1998  matt newville
       implicit  none
       include 'maxpts.h'
       include 'encod.h'
       integer  iop, ierr, nx, i
       double precision    x(*), xtmp(maxpts)
       double precision tmp, zero, one, half, quartr
       parameter(zero= 0.d0, one= 1.d0,half= 0.5d0,quartr=2.5d-1)
       double precision dgamma, dlgama, erf_xx
       external dgamma, dlgama, erf_xx
c
       ierr  = 0
       if (iop.eq.iexp) then
          do 20 i = 1, nx
             tmp  = max(-expmax, min(x(i), expmax) )
             x(i) = exp(tmp)
 20       continue
       elseif (iop.eq.ilog) then
          do 40 i = 1, nx
             if  ( x(i).gt.zero ) then
                x(i) = log(x(i))
             else
                ierr  = iop
                x(i) = zero
             end if
 40       continue
       elseif (iop.eq.ilog10) then
          do 42 i = 1, nx
             if  ( x(i).gt.zero ) then
                x(i) = log10(x(i))
             else
                ierr  = iop
                x(i) = zero
             end if
 42       continue
       elseif (iop.eq.isqrt) then
          do 60 i = 1, nx
             if  ( x(i).ge.zero ) then
                x(i) = sqrt(x(i))
             else
                ierr  = iop
                x(i) = zero
             end if
 60       continue
       elseif (iop.eq.iabs)  then
          do  80 i = 1, nx
             x(i) = abs(x(i))
 80       continue
       elseif (iop.eq.ineg)  then
          do 100 i = 1, nx
             x(i) = - x(i)
 100      continue
       elseif (iop.eq.isin)  then
          do 120 i = 1, nx
             x(i) = sin(x(i))
 120      continue
       elseif (iop.eq.icos)  then
          do 140 i = 1, nx
             x(i) = cos(x(i))
 140      continue
       elseif (iop.eq.itan)  then
          do 160 i = 1, nx
             x(i) = tan(x(i))
 160      continue
       elseif (iop.eq.iasin) then
          do 180 i = 1, nx
             if (abs(x(i)).le.one) then
                x(i) = asin(x(i))
             else
                ierr  = iop
                x(i) = zero
             end if
 180      continue
       elseif (iop.eq.iacos) then
          do 200 i = 1, nx
             if (abs(x(i)).le.one) then
                x(i) = acos(x(i))
             else
                ierr  = iop
                x(i) = zero
             end if
 200      continue
       elseif (iop.eq.iatan) then
          do 220 i = 1, nx
             x(i) = atan(x(i))
 220      continue
       elseif (iop.eq.itanh) then
          do 240 i = 1, nx
             x(i) = tanh(x(i))
 240      continue
       elseif (iop.eq.icoth) then
          do 242 i = 1, nx
             x(i) = one/tanh(x(i))
 242      continue
       elseif (iop.eq.icosh) then
          do 260 i = 1, nx
             x(i) = cosh(max(-expmax, min(x(i), expmax)))
 260      continue
       elseif (iop.eq.isinh) then
          do 280 i = 1, nx
             x(i) = sinh(max(-expmax, min(x(i), expmax)))
 280      continue
       elseif (iop.eq.jderiv) then
          xtmp(1) = x(2)  - x(1)
          do 300 i = 2, nx-1
             xtmp(i) = (x(i+1) - x(i-1))*half
 300      continue
          xtmp(nx) = x(nx)  - x(nx-1)
          do 305 i = 1, nx
             x(i) = xtmp(i)
 305      continue 
       elseif (iop.eq.jsmoo) then
          xtmp(1) =3*x(1)*quartr + x(2)*quartr
          do 320 i = 2, nx-1
             xtmp(i) = (x(i) + (x(i+1) + x(i-1))*half)*half
 320      continue
          xtmp(nx) = 3*x(nx)*quartr + x(nx-1)*quartr
          do 325 i = 1, nx
             x(i) = xtmp(i)
 325      continue 
       elseif (iop.eq.jasign)  then
          do 360 i = 1, nx
             tmp = abs(x(i))
             if (tmp.eq.zero) then
                x(i) =  zero
             elseif (tmp.eq.x(i)) then
                x(i) =  one
             else
                x(i) = -one
             end if
 360      continue
c external special functions
       elseif (iop.eq.jgamma)  then
          do  502 i = 1, nx
             tmp  = x(i)
             x(i) = dgamma(tmp)
 502      continue
       elseif (iop.eq.jlgamm)  then
          do  503 i = 1, nx
             tmp  = x(i)
             x(i) = dlgama(tmp)
 503      continue
       elseif (iop.eq.jerf)  then
          do  510 i = 1, nx
             tmp  = x(i)
             x(i) = erf_xx(tmp,0)
 510      continue
       elseif (iop.eq.jerfc)  then
          do  511 i = 1, nx
             tmp  = x(i)
             x(i) = erf_xx(tmp,1)
 511      continue
       elseif (iop.eq.jerfcx)  then
          do  512 i = 1, nx
             tmp  = x(i)
             x(i) = erf_xx(tmp,2)
 512      continue

       end if
       return
c end subroutine f1mth
       end
       subroutine f2mth( x, nx, y, ny, iop, ierr)
c
c two component math: x is overwritten by operator(x,y).
c if ( (negative number)**(fraction) ) is requested, x is unchanged.
c
c iop is an integer indication of which operation to perform.
c if iop = 0, then x is returned.
c
c  copyright (c) 1998  matt newville
       implicit none
       include 'maxpts.h'
       include 'encod.h'
       integer  ix, iy, i, newx
       double precision      x(*), y(*), xout(maxpts)
       double precision      xtmp, test
       integer   nx, ny, iop, ierr, nx1, ny1, nptstk
       external   nptstk
       ierr = 0
c decide number of points to write out
c     npstk = smaller of nx1, ny1 unless either of them is 1,
c             in which case it is the larger of the two.
c             (n = 1 implies a scalar )
       nx1  = nx
       ny1  = ny
       nx   = nptstk (nx1, ny1)
       if ((iop.eq.iadd).or.(iop.eq.jadd)) then
          do 20 i = 1, nx
             ix = min(nx1, i)
             iy = min(ny1, i)
             xout(i) = y(iy) + x(ix)
 20       continue
       elseif ((iop.eq.isub).or.(iop.eq.jsub)) then
          do 40 i = 1, nx
             ix = min(nx1, i)
             iy = min(ny1, i)
             xout(i) = y(iy) - x(ix)
 40       continue
       elseif (iop.eq.imul) then
          do 60 i = 1, nx
             ix = min(nx1, i)
             iy = min(ny1, i)
             xout(i) = y(iy) * x(ix)
 60       continue
       elseif (iop.eq.idiv) then
          do 80 i = 1, nx
             ix = min(nx1, i)
             iy = min(ny1, i)
             if (x(ix).eq.0)  then
                xout(i) = 0
                ierr  = iop
             else
                xout(i) = y(iy) / x(ix)
             end if
 80       continue
       elseif (iop.eq.iy2x) then
          do 100 i = 1, nx
             ix   = min(nx1, i)
             iy   = min(ny1, i)
             newx = int(x(ix))
             xtmp = newx
             if (x(ix).eq.0)  then
                xout(i) = 1
             elseif ( (y(iy).eq.0).and.(x(ix).gt.0) ) then
                xout(i) = 0
             elseif (y(iy).gt.0)  then
                test  = x(ix) * log(y(iy))
                if (test.gt.expmax) then
                   xout(i) = exp(expmax)
                elseif (test.lt.(-expmax)) then
                   xout(i) = exp(-expmax)
                else
                   xout(i) = y(iy)**x(ix)
                end if
             elseif ( (y(iy).lt.0) ) then
                test  = xtmp * log(-y(iy))
                if (test.gt.expmax) then
                   xout(i) = exp(expmax)
                elseif (test.lt.(-expmax)) then
                   xout(i) = exp(-expmax)
                else
                   xout(i) = y(iy)**newx
                end if
             end if
 100      continue
       elseif (iop.eq.jmin) then
cc          print*, ' f2mth jmin ' 
          do 120 i = 1, nx
             ix = min(nx1, i)
             iy = min(ny1, i)
             xout(i) = min(y(iy) , x(ix))
 120      continue
       elseif (iop.eq.jmax) then
cc          print*, ' f2mth jmax ' , nx, nx1, ny1
          do 140 i = 1, nx
             ix = min(nx1, i)
             iy = min(ny1, i)
             xout(i) = max(y(iy) , x(ix))
 140      continue
cc          print*, ' f2mth jmax: ', xout(1), xout(2)
       end if
c now overwrite x
       do 300 i = 1, nx
          x(i) = xout(i)
 300   continue
       return
c end subroutine f2mth
       end
       subroutine v1mth(x, nx, iop, ierr)
c
c one component math on a vector.  if any tests are failed, x is 
c returned.    iop indicates what operation to perform.
c
c copyright (c) 1998  matt newville
       include 'encod.h'
       double precision    x(*), xtmp
       integer iop, ierr, nx, i
       ierr = 0
       xtmp = 0
       if (nx.le.0) nx = 1
       if (iop.eq.jnpts) then
          xtmp = nx
       elseif (iop.eq.jceil) then
          xtmp  = x(1)
          do 20 i = 2, nx
             xtmp = max(xtmp, x(i))
 20       continue
       elseif (iop.eq.jfloor)  then
          xtmp  = x(1)
          do 40 i = 2, nx
             xtmp = min(xtmp, x(i))
 40       continue
       elseif (iop.eq.jvsum)  then
          xtmp  = 0
          do 60 i = 1, nx
             xtmp = xtmp + x(i)
 60       continue
       elseif (iop.eq.jvprod)  then
          xtmp  = 1
          do 80 i = 1, nx
             xtmp = xtmp * x(i)
 80       continue
       end if
c final array
       do 200 i = 2, nx
          x(i) = 0
 200   continue
       x(1) = xtmp
       nx   = 1
       return
c end subroutine v1tmh
       end

