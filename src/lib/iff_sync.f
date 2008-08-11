       subroutine iff_sync
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
c  fix integer code arrays icdsca and icdarr from encod so that:
c     1.  all variables come first
c     2.  all true constants come next (well, most ...)
c     3.  all other math expressions are ordered so that each
c         depends on only its predecessors.  this allows
c         single-pass, ordered decoding.
c
c arguments
c notes:
c  2. elements   1      -> nvar          of icdsca are variables
c     elements  nvar+1  -> nconst +nvar  of icdsca are constants
c  3. the parameters mxval and mcode should be >= maxval, micode.
c
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'encod.h'
       include 'pthpar.h'
       include 'feffit.h'
       save
       integer  iv, i, iv1, it1, icd, itmp, ix, k, j
       integer  nplast, nlast, npnext_max, npnext_swap
       integer  nxtmp, npmax
       parameter (npnext_max = maxheap_array - 3*maxsize_array)
cc       parameter (npnext_swap= maxheap_array -20*maxsize_array)
       logical  lswitch
c count number of scalars and arrays in use
c
cc         print*, 'top of sync'

c      
       nxscal = 0
       nxarr  = 0
c minimum npnext at which arrays will be reshuffled in order to
c eliminate erased arrays
       npnext_swap = maxheap_array * 6.5/8.0
       do 110 i = 1, maxsca
          if (scanam(i).ne.blank)  nxscal = i
 110   continue
       do 150 iv = 1, nxscal
          if (scanam(iv).eq.blank) then
             itmp = iv + 1
             call sswap(scanam(itmp), scanam(iv))
             call sswap(scafrm(itmp), scafrm(iv))
             call xswap(scalar(itmp), scalar(iv))
             call iaswap(icdsca(1,itmp), icdsca(1,iv), micode)
c now swap all references to these in the other icodes
             iv1 = jscale + iv
             it1 = jscale + itmp
             call icswap(iv1, it1)
          end if
 150   continue
c
c  count scalars again
       do 160 i = 1, maxsca
          if (scanam(i).ne.blank)  nxscal = i
 160   continue
c
       call setsca('&n_scalars', nxscal*1.d0)

       if (npnext.gt.npnext_max) then
          call echo(' *** warning: running low on space for arrays')
          call warn(1, ' ***          please clean up arrays ')
       endif

       do 830 i = 1, maxarr-1
          if ((narray(i).le.1).and.(arrnam(i).ne.undef_array))
     $         arrnam(i) = blank
          if (arrnam(i).ne.blank)  nxarr = i
 830   continue
      
cc get rid of empty arrays
       nxtmp = 0
       if (npnext .ge. npnext_swap) then
          do 250 iv = 1, nxarr
             if ((arrnam(iv).eq.blank).and.(narray(iv).le.0)) then
                do 240 itmp = iv+1, nxarr
                   if (arrnam(itmp).ne.blank) then
cc                   print*, ' swapping blank array at ' ,iv , 
cc     $                     ' with array at ', itmp, icdarr(1,iv)
                      nxtmp = nxtmp + 1
                      call sswap(arrnam(itmp), arrnam(iv))
                      call sswap(arrfrm(itmp), arrfrm(iv))
                      call xswap(arrmax(itmp), arrmax(iv))
                      call xswap(arrmin(itmp), arrmin(iv))
                      call iswap(narray(itmp), narray(iv))
                      call iswap(nparr(itmp), nparr(iv))
                      call iaswap(icdarr(1,itmp), icdarr(1,iv), micode)
                      call icswap(itmp, iv)
                      goto 242
                   endif
 240            continue 
 242            continue 
             end if
 250      continue
cc          print*, ' swapped arrays ', npnext, npnext_swap, nxarr, nxtmp
       endif
c
c  count arrays again
       nxarr  = 0
       nplast = 1
       do 860 i = 1, maxarr-1
          if (arrnam(i).ne.blank)  then 
             nxarr = i
             nplast= max(nplast,nparr(i))
             if (nplast.eq.nparr(i)) nlast = i
          endif
 860   continue
c       do i = 1, 25
c          print*, i, array(i)
c       enddo
       call setsca('&n_arrays', nxarr*1.d0)
       call setsca('&heap_used', npnext*1.d0/maxheap_array)

c------ 
c Step #0  put fitting variables first:  simple swap in place
       
       if (iprint.ge.17) then
          call echo(' lining up variables')
       endif
       itmp = 0
       do 1100 iv = 1, nxscal
c is_a_variable =   (icdsca(1,iv).eq.-1) 
          if (icdsca(1,iv).eq.-1) then
             itmp = itmp + 1
c if itmp .ne. iv, swap these two places (names, value, icodes)
             if (itmp.ne.iv) then
                call sswap(scanam(itmp), scanam(iv))
                call sswap(scafrm(itmp), scafrm(iv))
                call xswap(scalar(itmp), scalar(iv))
                call iaswap(icdsca(1,itmp), icdsca(1,iv), micode)
c now swap all references to these in the other icodes
                iv1 = jscale + iv
                it1 = jscale + itmp
                call icswap(iv1, it1)
             end if
          end if
 1100  continue
       nvarys =itmp
       call setsca('&n_guess', nvarys*1.d0)

c Step #1: put constant scalars second
c (an expression is constant unless its icode contains
c  a number between 1 and jconst:  'sqrt(5/2)' is a constant )
c   note 1: a 'set' scalar has the self-referential definition
c             icdsca(1,iv) = iv+jscale 
c           and should be treated as a constant
c   note 2: deep magic occurs when a "constant" is overwritten
c           by an ifeffit function.  A notable example is when
c           ff2chi and feffit overwrite 'reff' and 'degen'
c
       itmp = nvarys
       do 1300 iv = nvarys + 1, nxscal
          lswitch = icdsca(1,iv).ne.0
          if (icdsca(1,iv).ne.(iv+jscale)) then
             do 1230 i = 1, micode
                if (((icdsca(i,iv) .ge. 1) .and.
     $               (icdsca(i,iv) .lt. jconst))) lswitch =.false.
                if (icdsca(i,iv).eq.0) go to 1235
 1230        continue
 1235        continue
          end if
          if (lswitch) then
             itmp = itmp + 1
c if itmp .ne. iv, swap these two places (names, value, icodes)
             if (itmp.ne.iv) then
cc                print*, 'A swapping scalars  ' ,scanam(itmp)(1:30),
cc     $               scanam(iv)(1:30)
                call sswap(scanam(itmp), scanam(iv))
                call sswap(scafrm(itmp), scafrm(iv))
                call xswap(scalar(itmp), scalar(iv))
                call iaswap(icdsca(1,itmp), icdsca(1,iv), micode)
c now swap all references to these in the other icodes
                iv1 = jscale + iv
                it1 = jscale + itmp
                call icswap(iv1, it1)
             end if
          end if
 1300  continue
       nconst  = itmp - nvarys
       call setsca('&n_scalars_set', nconst*1.d0)
       call setsca('&n_scalars_def', (nxscal-nvarys-nconst)*1.d0)

c Step #3  put the math expressions in order
c          a simple switch-in-place, with recovery
       do 1600  ix = nconst + nvarys + 1, nxscal
          do 1580 iv = nconst + nvarys +1, nxscal
             lswitch = .false.
             do 1510 i = 1, micode
                icd = icdsca(i,iv)
                if ( icd.eq.0 ) go to 1520
                if ( (icd.gt.iv+jscale).and.
     $               (icd.lt.jconst))  lswitch  = .true.
 1510         continue
 1520         continue
c if itmp .gt. iv, swap these two places (names, value, icodes)
             if (lswitch) then
                itmp = itmp + 1
                if ((itmp.gt.iv).and.(itmp.le.nxscal)) then
cc                   print*, 'XX swapping scalars  ' ,scanam(itmp)(1:30),
cc     $                  scanam(iv)(1:30)
                   call sswap(scanam(itmp), scanam(iv))
                   call sswap(scafrm(itmp), scafrm(iv))
                   call xswap(scalar(itmp), scalar(iv))
                   call iaswap(icdsca(1,itmp), icdsca(1,iv), micode)
c now swap all references to these in the other icodes
                   iv1 = jscale + iv
                   it1 = jscale + itmp
                   call icswap(iv1, it1)
                end if
             end if
 1580      continue
 1600   continue

c
c Step #4: move constant arrays to top of pile
c         constant arrays have icdarr(1,in) = in, icdarr(2,in) = 0
       itmp = 0
ccc       if (npnext .ge. npnext_swap) then
       if (npnext .ge. 1) then
          do 2500 iv = 1, nxarr
             lswitch = (icdarr(1,iv).eq.iv) .and. (icdarr(2,iv).eq.0)
c      c          print*, 'SYNC:array iv ',iv,arrnam(iv)(1:30)
c      c          print*, 'SYNC: ', nparr(iv), narray(iv),
c      c     $         icdarr(1,iv),icdarr(2,iv)
             if (lswitch) then
                itmp = itmp + 1
c      if itmp .ne. iv, swap these two places (names, value, icodes)
                if (itmp.ne.iv) then
cc                   print*, ' swapping constant array at ' ,iv ,
cc     $                  ' with array at ', itmp, icdarr(1,iv)
                   call sswap(arrnam(itmp), arrnam(iv))
                   call sswap(arrfrm(itmp), arrfrm(iv))
                   call xswap(arrmax(itmp), arrmax(iv))
                   call xswap(arrmin(itmp), arrmin(iv))
                   call iswap(narray(itmp), narray(iv))
                   call iswap(nparr(itmp),  nparr(iv))
                   call iaswap(icdarr(1,itmp), icdarr(1,iv), micode)
c      now swap all references to these in the other icodes
                   call icswap(itmp, iv)
                end if
             end if
 2500     continue
       endif
       ncarr = itmp
       call setsca('&n_arrays_set', ncarr*1.d0)
       call setsca('&n_arrays_def', (nxarr-ncarr)*1.d0)
cc
c       print*,  'sync : ncarr nxarr = ', ncarr, nxarr, itmp
c
c Step #5  put the math expressions in order
c          a simple switch-in-place, with recovery
       do 3600  ix = ncarr + 1, nxarr
          do 3580 iv = ncarr +1, nxarr
             lswitch = .false.
             do 3510 i = 1, micode
                icd = icdarr(i,iv)
                if ( (icd.eq.0).or.(icd.eq.iv))  go to 3520
                if ( (icd.gt.iv).and.(icd.lt.jscale))
     $               lswitch  = .true.
 3510        continue
 3520        continue
c     c if itmp .gt. iv, swap these two places (names, value, icodes)
             if (lswitch) then
                itmp = itmp + 1
                if ((itmp.gt.iv).and.(itmp.le.nxarr)) then
c                   print*, ' swapping arrays ', iv, itmp
c                   print*, itmp, narray(itmp), arrnam(itmp)(1:20)
c                   print*, iv,   narray(iv),   arrnam(iv)(1:20)

                   call sswap(arrnam(itmp), arrnam(iv))
                   call sswap(arrfrm(itmp), arrfrm(iv))
                   call xswap(arrmax(itmp), arrmax(iv))
                   call xswap(arrmin(itmp), arrmin(iv))
                   call iswap(narray(itmp), narray(iv))
                   call iswap(nparr(itmp),  nparr(iv))
                   call iaswap(icdarr(1,itmp), icdarr(1,iv), micode)
c     now swap all references to these in the other icodes
                   call icswap(itmp, iv)
                   call fixarr(itmp,arrnam(itmp),narray(itmp), 0)
                   call fixarr(iv  ,arrnam(iv),  narray(iv),   0)
                end if
             end if
 3580     continue
 3600  continue
c   
c count string variables
       itmp = 0
       do 4300 i = 1, maxtxt
          if ((txtnam(i).ne.blank).and.(text(i).ne.blank)) itmp = itmp+1
 4300  continue 
       call setsca('&n_strings', itmp*1.d0)
 
c       
c re-evaluate all the ifeffit variables from the newly sorted values
       call synvar
c end subroutine iff_sync
       end
       subroutine synvar
c   revaluate all variables, assumed to be in order from sync
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'encod.h'
       include 'feffit.h'
       save
       integer i, ntmp, iu

cc       do 500 iu =  1,2
          do 10 i = 1, maxpts
             tmparr(i) = 0
 10       continue 
          do 200 i = nvarys + 1, nxscal 
             if (icdsca(1,i) .gt. 0) then
                tmparr(1)  = scalar(i)
                if (icdsca(1,i).ne.(i+jscale)) then
                   ntmp = 0
                   call decod(icdsca(1,i),micode, consts, scalar,
     $                  array, narray, nparr,
     $               maxsize_array, maxarr, ntmp, tmparr)
                   scalar(i) = tmparr(1)
                end if
             endif
 200      continue
          do 300 i = ncarr+1, nxarr
             if (icdarr(1,i) .gt. 0) then
                ntmp = 0
                call decod(icdarr(1,i),micode, consts, scalar,
     $               array, narray, nparr,
     $               maxsize_array, maxarr,  ntmp, tmparr)
                call set_array_index(i,tmparr,ntmp)
             end if
 300      continue
 500   continue 
       return
       end
       subroutine icswap(inew, iold)
c switch occurance of inew and iold  in all encoded expressions
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'encod.h'
       include 'pthpar.h'
       include 'feffit.h'
       save
       integer inew, iold, ip, j, i
c
c scalars
       do 240 j = 1, maxsca
          do 220 i = 1, micode
             if (icdsca(i,j).eq.inew)  then
                icdsca(i,j) = iold
             elseif (icdsca(i,j).eq.iold)  then
                icdsca(i,j) = inew
             elseif (icdsca(i,j).eq.0)  then
                go to 230
             end if
 220      continue
 230      continue 
 240   continue
c arrays
       do 340 j = 1, maxarr-1
          do 320 i = 1, micode
             if (icdarr(i,j).eq.inew)  then
                icdarr(i,j) = iold
             elseif (icdarr(i,j).eq.iold)  then
                icdarr(i,j) = inew
             elseif (icdarr(i,j).eq.0)  then
                go to 330
             end if
 320      continue
 330      continue
 340   continue
c path parameters
       do 560 ip = 1, mpaths
          do 540 j = 1, mpthpr
             do 520 i = 1, micode
                if (icdpar(i,j,ip).eq.inew)  then
                   icdpar(i,j,ip) = iold
                elseif (icdpar(i,j,ip).eq.iold)  then
                   icdpar(i,j,ip) = inew
                elseif (icdpar(i,j,ip).eq.0)  then
                   go to 530
                end if
 520         continue
 530         continue
 540      continue
 560   continue
       return
       end
       subroutine sswap(s1, s2)
c  swap 2 strings
       character*512 s, s1*(*), s2*(*)
       s  = s1
       s1 = s2
       s2 = s
       return
       end
       subroutine xswap(x1, x2)
c  swap 2 dp constants
       double precision x, x1, x2
       x  = x1
       x1 = x2
       x2 = x
       return
       end
       subroutine iswap(i1, i2)
c  swap 2 integers
       integer  i, i1, i2
       i  = i1
       i1 = i2
       i2 = i
       return
       end
       subroutine iaswap(i1, i2, n)
c  swap 2 integer arrays, of length n
       integer i1(*), i2(*), i, j, n
       do 10 j = 1, n
          i     = i1(j)
          i1(j) = i2(j)
          i2(j) = i
 10    continue 
       return
       end
c       subroutine xaswap(array, narray, nparr, i1, i2, n)
cc  swap 2 dp arrays, of length n
c       double precision  x1(*), x2(*), x
c       integer j, n
c       do 10 j = 1, n
c          x     = x1(j)
c          x1(j) = x2(j)
c          x2(j) = x
c 10    continue 
c       return
c       end

