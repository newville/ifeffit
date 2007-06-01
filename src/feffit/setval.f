       subroutine setval(n1, n2, icdval, mvals, micode, consts,
     $      mconst, vals, icloc, mloc, md,  ixlocl,  id)
c simple loop through resetting values using eval.
       implicit none
       integer  mconst, mvals, micode, mloc, md
       integer  ixlocl, i, id, n1, n2
       double precision  consts(mconst), vals(mvals), old, eval
       integer  icdval(micode, mvals), icloc(micode,mloc,md)
       external eval
       do 10 i = n1, n2
          old   = vals(i)
          vals(i) = eval(icdval, mvals, micode, consts,
     $         mconst, vals, old, icloc,
     $         mloc, md,  ixlocl,  id, i )
 10    continue
       return
       end
