       subroutine fitck2
c
c  this routine is part of feffit: does some  checking of math
c    expressions for path parameters for the "reasonableness"
c    of their initial values,  so that deltar is not 100.0, etc.
c  note: this needs to be called *after* the feff.dat info is read
c
c      copyright 1993 university of washington         matt newville
c----------------------------------------------------------------
       include 'fitcom.h'
       double precision  par, pmax(mpthpr), small, decod
       parameter (small = 1.d-3)
       character*20  setchr
       integer nstart, id, ipath, i, il, inpath, jfeff, juser
       external decod
c  these are the magnitudes of the upper limits for "reasonable"
c  initial values -- they're pretty darn big
       pmax(jps02)  = 1.d6
       pmax(jpe0)   = 2.d1
       pmax(jpdelr) = 5.d-1
       pmax(jpsig2) = 3.d-1
       pmax(jpei)   = 2.d1
       pmax(jpdpha) = 1.d2
       pmax(jp3rd)  = 2.d-1
       pmax(jp4th)  = 1.d-1
       pmax(9)      = 1.d0
       pmax(10)     = 1.d0
c----------------------------------------------------------------------
c  evaluate the non-variable values
c       print*, ' fitck2!!'
       nstart = nconst + numvar + 1
       do 1000 id  = 1, ndata
c    first all the "set values"
          call setval(nstart,nmathx,icdval,maxval,micode,
     $      consts,mconst,values,icdloc,mlocal,mdata,ixlocl,id)
c  now test the values of the path parameters for sane guesses
c  also does error checking of whether all the path info is there, so
c  that such checks aren't needed in fitfun/fitout
          do 950 ipath = 1, mdpths
             inpath = jdtpth(ipath, id)
             if (inpath.gt.0) then
                jfeff  = jpthff(inpath)
cc                print*, ' JFEFF  = ', jfeff, inpath, ipath, id
                if (jfeff.le.0) then
                   call echo(' jfeff < 0 ')
                   jdtpth(ipath,id) = -1
                   go to 940
                end if
                juser  = jdtusr(ipath, id)
                if (refpth(jfeff).le.small) then
                   jdtpth(ipath,id) = -1
                   jpthff(inpath)   = -1
                   go to 940
                end if
                consts(4) =  refpth(jfeff)
                do 920 i = 1, mpthpr
                   par = abs(decod(icdpar(1, i, inpath),
     $                  micode, consts, values, defalt(i)))
                   if (par.gt.pmax(i)) then
                      write (messg,'(a,i3)') 'for path ',juser
                      if (ndata.gt.1) then
                         write (setchr,'(a,i3)') ' of data set ',id
                         call append(messg,setchr,il)
                      endif
cc                      print*, ' parnam: ', i,  parnam(i), par
                      call finmsg(3300,parnam(i),messg,-1)
                   end if
 920            continue
             endif
 940         continue
 950      continue
 1000  continue


       return
c  end subroutine fitck2
       end
