       subroutine kkmclf(npts, e, finp, fout)
c  forward (f'->f'') kk transform, using  mclaurin series algorithm
c  arguments: 
c    npts   size of arrays to consider 
c    e      energy array *must be on an even grid* [npts]  (in)
c    finp   f'     array                           [npts]  (in)
c    fout   f''    array                           [npts]  (out)
c  notes  fopi = 4/pi
       implicit none
       double precision  e(*), finp(*), fout(*)
       double precision  factor, ei2, de2, fopi, zero, tiny
       parameter(fopi = 1.273239544735163d0, zero = 0.d0, tiny=1.d-20)
       integer   npts, i, j, k, ioff, nptsk
       
       if (npts.ge.2) then 
          factor = fopi * (e(npts) - e(1)) / (npts - 1)
          nptsk  = npts / 2
          do 100 i=1, npts
             fout(i) = zero
             ei2     = e(i) * e(i)
             ioff    = mod(i,2) - 1
             do 50 k = 1, nptsk
                j    = k + k + ioff
                de2  = e(j)*e(j) - ei2
                if (abs(de2).le.tiny) de2 = tiny
                fout(i) = fout(i) + finp(j) / de2
 50          continue 
             fout(i) = factor * fout(i) * e(i)
 100      continue 
       end if
       return
c end subroutine kkmclf
       end
