       subroutine fixicd(icdval, maxval, micode, jconst, jxlocl,
     $               vnames, xval, nvar, nconst, ntotal,io2n)
c
c  fix integer code arrays from encod so that :
c     1.  all variables come first
c     2.  all true constants come next (well, most ...)
c     3.  all other math expressions are ordered so that each
c         depends on only its predecessors.  this allows
c         single-pass, ordered decoding.
c
c arguments
c   icdval  integer array codes from encod (micode, maxval)  (in/out)
c   maxval  dimension of icdval, vnames, xval                (in)
c   micode  dimension of icdval                              (in)
c   jconst  integer placeholder for constants (from encod)   (in)
c   jxlocl  integer placeholder for locals                   (in)
c   vnames  array of variable names                          (in/out)
c   xval    array of variables values/initial guesses        (in/out)
c   nvar    number of variables                              (out)
c   nconst  number of constants                              (out)
c   ntotal  total number of named parameters                 (out)
c   io2n    map    io2n(iold) = inew                         (out)
c notes:
c  1. jxlocl > jconst !!   jconst = 2000, jxlocl = 5000 at this writing.
c  2. elements   1      -> nvar          of icdval are variables
c     elements  nvar+1  -> nconst +nvar  of icdval are constants
c  3. the parameters mxval and mcode should be >= maxval, micode.
c  4. jfake1 and jfake2 are for the "fake constants" (like reff)
c     hardwired in feffit/encod to be held in the constant array,
c     even though their values are continually overwritten.  I don't
c     want to include them in the  "true constants" list here.
c
       integer  mxval , mcode, micode, maxval, jconst, jxlocl
       integer  icdval(micode, maxval), io2n(maxval), nvar, nconst
       double precision      xval(maxval),     zero
       character*(*)   vnames(maxval)
       parameter (mxval = 2048, mcode = 256, zero = 0.d0)
       integer    jfake1 ,  jfake2
       parameter (jfake1 = 4, jfake2 = 8)
       character*64 vnew(mxval), vorig(mxval)
       integer  icdnew(mcode, mxval), iold(mxval), inew(mxval)
       integer  io, in, ntotal, icd, ncandv, jv
       integer  iv, ivold, ivnew, itmp, i
       integer   jdebye, jeins, jeins2
       parameter(jdebye=-120, jeins =-121, jeins2=-122)

       double precision  xnew(mxval), xxtmp
c      
       do 80 i = 1, maxval
          vnew(i) = ' '
          vorig(i)= vnames(i)
          xnew(i) = zero
          if (vnames(i).ne.' ')  ntotal = i
 80    continue
c
c reorder #1:  put variables 1st
      ivnew = 0
      do 100 iv = 1, ntotal
         if (icdval(1,iv).eq.-1) then
            ivnew        = ivnew + 1
            vnew(ivnew)  = vnames(iv)
            xnew(ivnew)  = xval(iv)
            inew(iv)     = ivnew
            iold(ivnew)  = iv
         end if
 100  continue
      nvar = ivnew
c
c reorder #2: put  constants 2nd
c   if                    1  <= icdval(i) <= jconst
c   or if    jfake1 + jconst <= icdval(i) <= jfake2 jconst
c   then its not a constant
      do 150 iv = 1, ntotal
         if ( (icdval(1,iv).ne.-1).and.
     $        (icdval(1,iv).le.jxlocl) ) then
            icd = 0
            do 130 i = 1, micode
               if (icdval(i,iv).eq.0) go to 135
               if ( (icdval(i,iv).eq.jdebye) .or.
     $              (icdval(i,iv).eq.jeins)  .or.
     $              (icdval(i,iv).eq.jeins2) ) icd = icd + 1
               if ( (icdval(i,iv).ge.1).and.
     $              (icdval(i,iv).lt.jconst) )  icd = icd + 1
               if ( (icdval(i,iv).ge.(jfake1+jconst)).and.
     $              (icdval(i,iv).le.(jfake2+jconst)) ) icd = icd + 1
 130        continue
 135        continue
            if (icd.eq.0) then
               ivnew       = ivnew + 1
               vnew(ivnew) = vnames(iv)
               xnew(ivnew) = xval(iv)
               inew(iv)    = ivnew
               iold(ivnew) = iv
            endif
         end if
 150  continue
      ncandv  = ivnew
      nconst  = ivnew - nvar
c
c reorder #3:  now put "local variables", all of which are marked with
c  an initial element larger that jxlocl that is much greater than
c  jconst (jxlocl = 5000, jconst = 2000  at this writing)
      do 240 iv = 1, ntotal
         if (icdval(1,iv).gt.jxlocl) then
            ivnew       = ivnew + 1
            vnew(ivnew) = vnames(iv)
            xnew(ivnew) = xval(iv)
            inew(iv)    = ivnew
            iold(ivnew) = iv
         endif
 240  continue
c
c add all the other math expressions to the new arrays.
c without worrying about order
      do 400 iv = 1, ntotal
         icd = 0
         do 360 i = 1, micode
            if (icdval(i,iv).eq.0) go to 370
            if ((icdval(i,iv).ge.1).and.
     $           (icdval(i,iv).lt.jconst ) )  then
               icd = icd + 1
               go to 370
            end if
            if ( (icdval(i,iv).eq.jdebye) .or.
     $           (icdval(i,iv).eq.jeins)  .or.
     $           (icdval(i,iv).eq.jeins2) ) then
               icd = icd + 1
               go to 370
            end if

            if ((icdval(i,iv).ge.(jfake1+jconst)).and.
     $          (icdval(i,iv).le.(jfake2+jconst)) ) then
               icd = icd + 1
               go to 370
            end if
 360     continue
 370     continue
         if (icd.ne.0) then
            ivnew        = ivnew + 1
            vnew(ivnew)  = vnames(iv)
            xnew(ivnew)  = xval(iv)
            inew(iv)     = ivnew
            iold(ivnew)  = iv
         endif
 400  continue
c
c       print*, ' new variable order: ', ntotal, nconst, nvar
c       do i = 1, ntotal
c          print*, i, ' ',vnew(i)(1:20)
c       enddo
c now replace all occurances of iv with inew(iv) (= ivnew) in icdval.
      do 540 iv = 1, ntotal
         ivold = iold(iv)
         do 520 i = 1, micode
            if ((icdval(i,ivold).ge.1).and.
     $           (icdval(i,ivold).lt.jconst ) ) then
               icdnew(i,iv) = inew(icdval(i,ivold))
            else
               icdnew(i,iv) = icdval(i,ivold)
            endif
 520    continue
 540  continue
      do 600 iv = 1, maxval
         vnames(iv) = vnew(iv)
         xval(iv)   = xnew(iv)
         xnew(iv)   = zero
         vnew(iv)   = ' '
         iold(iv)   = 0
         inew(iv)   = 0
         do 580 i = 1, micode
            icdval(i,iv) = icdnew(i,iv)
 580     continue
 600  continue
c
c finally, put the math expressions in order
c a simple switch-in-place, with recovery
      do 900 iv = ncandv+1, ntotal
         itmp = ncandv
         do 750 i = 1, micode
            icd = icdval(i,iv)
            if (icd.eq.0) go to 760
            if ((icd.ge.ncandv).and.
     $           (icd.lt.jconst))  itmp = max(icd,itmp)
 750     continue
 760     continue
         if (itmp.gt.iv) then
            ivold = iv
            ivnew  = itmp
            do 780 i = 1, micode
               icdnew(i,1)     = icdval(i,ivnew)
               icdval(i,ivnew) = icdval(i,ivold)
               icdval(i,ivold) = icdnew(i,1)
 780        continue
            vnew(1)       = vnames(ivnew)
            vnames(ivnew) = vnames(ivold)
            vnames(ivold) = vnew(1)
            xxtmp         = xval(ivnew)
            xval(ivnew)   = xval(ivold)
            xval(ivold)   = xxtmp
            do 850 jv = ncandv+1, ntotal
               do 820 i = 1, micode
                  if (icdval(i,jv).eq.0) go to 830
                  if (icdval(i,jv).eq.ivold) then
                     icdval(i,jv) =  ivnew
                  elseif (icdval(i,jv).eq.ivnew) then
                     icdval(i,jv) =  ivold
                  endif
 820           continue
 830           continue
 850        continue
         end if
 900  continue
c
c now re-map old to new and return
      do 1000 io = 1, ntotal
         do 980 in = 1, ntotal
            if (vorig(io).eq.vnames(in)) then
               io2n(io) = in
               go to 990
            endif
 980     continue
 990     continue
 1000 continue
c
      return
c end subroutine fixicd
      end
