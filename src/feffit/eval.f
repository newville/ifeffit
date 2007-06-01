       double precision function eval(icode,mvals,micode,
     $      consts,mconst, vals,old,
     $      icloc,mloc,md,ixlcl, id, ieval)
c
c  evaluate integer codes of math expressions for "set" and "local"
c  valuess.  includes decision of whether the named value is global
c  ("set") or "local" to the data set.
c
c  notes:
c    1. the first m element of "i(1,i)" are i(1,i) to i(m,i).
c    2. the first element of the encoded integer arrays holds an
c       important clue about the nature of the math expression:
c        icode(1,i) = -1      variable
c        icode(1,i) =  0      unused
c        icode(1,i) >  0      user-defined function ("global")
c        icode(1,i) >  ixlcl  "local"
       integer  mconst, mvals, micode, mloc, md
       integer  ixlcl, ieval, jlcl, id
       double precision  consts(mconst), vals(mvals), old, decod
       integer  icode(micode, mvals), icloc(micode,mloc,md)
       external decod
       eval = old
       if (icode(1,ieval).gt.ixlcl) then
          jlcl = icode(1,ieval) - ixlcl
          eval = decod(icloc(1,jlcl,id), micode,consts,vals,old)
       elseif (icode(1,ieval).gt.0)  then
          eval = decod(icode(1,ieval),   micode,consts,vals,old)
       endif
       return
       end
