      subroutine fitchk
c
c      this routine does some error checking of the math expressions
c      for feffit.  the expressions have already been encoded in
c      fitinp, and the math syntax has been checked in encod, but not
c      all the checks could be done until all the input files have been
c      read in. done here are checks so that:
c        1.  all variables named in the math expresions must
c            be defined as either variables or functions.
c        2.  all defined variables and functions are used, or a
c            warning will be given.
c        3.  all encoded math expressions can be reliably decoded.
c            all math expressions are actually evaluated here.
c
c      also done here is the use of the zeroth (0th) path as the default
c      expression for the various path parameters. the 0th path will be
c      used here to overwrite those path parameters that were not
c      explicitly given in the input file.
c
c      copyright 1993 university of washington         matt newville
c
c----------------------------------------------------------------
       include 'fitcom.h'
c  from encod!
       integer   jconst, io2n(maxval)
       integer  iv, id, i, j, ip, jv, ilcl, inpath, jpar
       integer  idata, inpth0, idpath, ipar, nvar
       parameter (jconst = 8192)
c----------------------------------------------------------------------
c
c fix integer codes to put variables first, then constants, then
c ordered math expressions, so that a single pass will correctly
c evaluate all "set" values
c notes:  puts "local" variables  after constants
c         doesn't check for pathological recursion
       nvar = numvar
       call fixicd(icdval, maxval, micode, jconst, ixlocl,
     $      vnames, values, nvar, nconst, nmathx, io2n)
       if (nvar.ne.numvar)  call finmsg(3200,' ',' ',0)
c update xguess to reordered values!
       do 150 iv = 1, numvar
          xguess(iv) = values(iv)
 150      continue
c
       do 360 id = 1, ndata
          do 340 j = 1, mlocal
             do 320 i = 1, micode
                if (icdloc(i,j,id).eq.0) go to 325
                if ((icdloc(i,j,id).gt.0).and.
     $               (icdloc(i,j,id).le.nmathx) )
     $               icdloc(i,j,id) = io2n(icdloc(i,j,id))
 320         continue
 325         continue
 340      continue
 360   continue
c
       do 860 ip = 1, mpaths
          do 840 j = 1, mpthpr
             do 820 i = 1, micode
                if (icdpar(i,j,ip).eq.0) go to 825
                if ((icdpar(i,j,ip).gt.0).and.
     $               (icdpar(i,j,ip).le.nmathx))
     $               icdpar(i,j,ip) = io2n(icdpar(i,j,ip))
 820         continue
 825         continue
 840      continue
 860   continue
c
c  check that variables named in math expressions are defined
       do 1000 iv = 1, nmathx
          if ( (vnames(iv).ne.' ').and.(icdval(1,iv).eq.0) )
     $         call finmsg(3220,vnames(iv),' ',0)
 1000  continue
c
c  check that the variables and user-defined functions are used.
c  expection for '_bkg_#' - these won't show up in other user-def fun's
       do 2000 iv = 1, nmathx
          if ( vnames(iv)(1:6).ne.'_bkg_#' ) then
c     check other user-defined functions and local values
             do 1600  jv = 1, nmathx
                if ((icdval(1,jv).gt.0).and.
     $               (icdval(1,jv).le.ixlocl)) then
                   do 1500 i = 1, micode
                      if (icdval(i,jv).eq.iv) go to 1990
 1500              continue
                elseif(icdval(1,jv).gt.ixlocl) then
                   ilcl = icdval(1,jv) -ixlocl
                   do 1560 id = 1, ndata
                      do 1530 i = 1, micode
                         if (icdloc(i,ilcl,id).eq.iv) go to 1990
 1530                 continue
 1560              continue
                endif
 1600        continue
c     check path parameters
             do 1850  inpath = 1, mpaths
                do 1820  jpar = 1, mpthpr
                   do 1800 i   = 1, micode
                      if (icdpar(i,jpar,inpath).eq.iv) go to 1990
 1800              continue
 1820           continue
 1850        continue
             call finmsg(3240,vnames(iv),' ',-1)
          end if
 1990     continue
 2000  continue
c
c  load default path params from the Oth  path.  copy 0th path param
c  to all non-explicitily given path params for that data set.
       do 4200 idata = 1, ndata
          inpth0  = jdtpth(0,idata)
c   if a 0th path is given for this data set...
          if (inpth0.le.0)  go to 4190
c   check all paths for this data set ...
          do 4150 idpath = 1, mdpths
             inpath = jdtpth(idpath, idata)
             if (inpath.gt.0) then
c   that if a zeroth path param was given ***and***
c   an explicit path param was ***not*** given ...
                do 4100 ipar = 1, mpthpr
                   if ((icdpar(1,ipar,inpath).eq.0).and.
     $                  (icdpar(1,ipar,inpth0).ne.0))   then
c   that the zeroth path expression be copied.
                      do 4050 j = 1, micode
                         icdpar(j,ipar,inpath) = icdpar(j,ipar,inpth0)
 4050                 continue
                   endif
 4100           continue
             endif
 4150        continue
 4190     continue
 4200  continue
c
c  check the newly encoded expressions by evaluating them.
c  evaluate the user defined values
       id  = 1
       call setval(1,nmathx,icdval,maxval,micode,
     $      consts,mconst,values,icdloc,mlocal,mdata,ixlocl,id)
       return
c  end subroutine fitchk
       end
