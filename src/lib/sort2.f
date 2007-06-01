       subroutine sort2(n, ra, rb)
c heap sort real array ra of length n to ascending order,
c and make the corresponding rearrangement to rb.
       implicit none
       integer n, l, ir, i, j
       double precision ra(*), rb(*), xa, xb
       l  = n / 2 + 1
       ir = n
c heap creation phase
c index l is decremented from its initial value down to 1
 10    continue
       if (l.gt.1) then
          l  = l - 1
          xa = ra(l)
          xb = rb(l)
c heap selection phase
c index ir is decremented from its initial value to to 1
       else
          xa = ra(ir)
          xb = rb(ir)
          ra(ir) = ra(1)
          rb(ir) = rb(1)
          ir = ir - 1
          if (ir.eq.1) then
             ra(1) = xa
             rb(1) = xb
             go to 50
          end if
       end if
c sift down xa
       i = l
       j = l + l
 20    continue
       if (j.le.ir) then
c found better low element
          if ((j.lt.ir).and.(ra(j).lt.ra(j+1))) j = j + 1
c demote xa
          if (xa.lt.ra(j)) then
             ra(i) = ra(j)
             rb(i) = rb(j)
             i = j
             j = j + j
          else
             j = ir + 1
          end if
c ends the sift-down
          go to 20
       end if
c restore xa and xb
       ra(i) = xa
       rb(i) = xb
       go to 10
c      return
 50    continue

       return
c  end subroutine sort2
       end
       integer function sort_xy(e, x, n, delta)
c
c check and sort a pair of energy/xmu data to 
c ensure that energy is monotonically increasing
c
c returns 0 for data that needs no sort or rearrangement
c         1 if sorting or rearrangement were needed

       implicit none
       double precision e(*), x(*), de, delta, dm
       integer n, i, k, j
       logical sort, near

       near = .false.
       sort = .false.
cc       print*, ' in sort_xy '
c first, look for problems
       do 10 i =  1, n-1
          de = e(i+1)-e(i)
          if (de.lt.0)    sort = .true.
 10    continue 
c
cc       print*, ' in sort_xy ', sort, near

c if needed, sort
       if (sort) call sort2(n, e, x)
c
c if needed, handle coincident x points
c by slightly incrementing values
       do 100 i =  1, n-1
          de = e(i+1)-e(i)
          if (abs(de).lt.delta) then
             near = .true.
             dm   = delta*10.d0
             if (i .eq. n-1.and.i.ge.2) then
                dm = max(delta,(e(i)-e(i-1))*0.01d0)
             else
                dm = max(delta,(e(i+2)-e(i))*0.01d0)
             endif
             e(i+1) =  e(i) + dm
cc             print*, ' moving energy point ', i, e(i+1)
          endif
 100    continue 
       sort_xy = 0
       if (sort.or.near) sort_xy = 1
       return 
       end




