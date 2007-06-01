      logical function iscomm(str)
c true if str is a comment line or blank line, false otherwise
      character*(*) str
      iscomm = ((str.eq.' ') .or. (index('*%#',str(1:1)).ne.0))
      return
      end


       subroutine sort2i(n, ira1, ira2)
c
c      sort an array ira1 of length n into ascending order,
c      while making the corresponding rearrangement to rb.
c      the sorting is done by the heapsort algorithm
c
       integer  ira1(n), ira2(n)
       l  = n / 2 + 1
       ir = n
c
c   index l will be decremented from its initial value down to 1
c   during the hiring phase (heap creation). Once l reaches 1, the
c   index ir will be decremented from its initial value to to 1
c   during the retirement-and-promotion (heap selection) phase.
c
 10    continue
c                                           heap creation phase
          if (l.gt.1) then
               l      = l - 1
               ia1    = ira1(l)
               ia2    = ira2(l)
c                                           heap selection phase
          else
               ia1    = ira1(ir)
               ia2    = ira2(ir)
               ira1(ir) = ira1(1)
               ira2(ir) = ira2(1)
               ir     = ir - 1
               if (ir.eq.1) then
                    ira1(1) = ia1
                    ira2(1) = ia2
                    go to 50
               end if
          end if
c                                           set up to sift down ia1.
          i = l
          j = l + l
c                                           do while j.le.ir
 20       continue
          if (j.le.ir) then
c                                           better low element
              if (j.lt.ir) then
                    if ( ira1(j).lt.ira1(j+1) )j = j + 1
              end if
c                                           demote ia1
              if (ia1.lt.ira1(j)) then
                    ira1(i) = ira1(j)
                    ira2(i) = ira2(j)
                    i     = j
                    j     = j + j
c                                           terminate the sift-down
              else
                    j     = ir + 1
              end if
              go to 20
          end if
c                                           put ia1, ia2 into slots
          ira1(i) = ia1
          ira2(i) = ia2
          go to 10
c  return
 50    continue
       return
c  end subroutine sort2i
       end
