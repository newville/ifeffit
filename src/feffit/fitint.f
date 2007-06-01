       subroutine fitint
c
c      initialize the common blocks in feffit at runtime.
c      see also fitbdt, which does this as block data
c
c      copyright 1993 university of washington         matt newville
c----------------------------------------------------------------------
       include 'fitcom.h'
c  temporary parameters for legths of multiple-dimensioned arrays
       integer lwfft, i, j, k
       parameter (lwfft  = (4*maxpts+15)   )
c
c  single values
       frminp = ' '
       frmout = ' '
       asccmt = '# '
       if (vaxflg) asccmt = '##'
       inpfil = 'feffit.inp'
       mdocxx =  0
       xnidp  =  zero
       inform =  0
       ndata  =  0
       rlast  =  10.d0
       cormin =  0.25d0
       rwght1 =  15.d0
       rwght2 =  25.d0
       tranq  =  2.d0
       allout = .true.
       final  = .false.
       rm2flg = .false.
       dphflg = .false.
       kspcmp = .false.
       kspout = .true.
       rspout = .true.
       bkgout = .true.
       qspout = .true.
       pcout  = .false.
       chkdat = .true.
       prmout = .true.
       noout  = .false.
       nofit  = .false.
       degflg = .true.
       iprint = 0
       chisqr = zero
       usrtol = one
       numvar = 0
       nvuser = 0
       ifxvar = 0
       ierbar = 0
       nerstp = 1
       ixpath = 0
       nmathx = 0
       nconst = 0
c
c loop over mdata
       do 300 i = 1, mdata
          skey(i)   = ' '
          outfil(i) = '_fit'
          chifil(i) = ' '
          bkgfil(i) = ' '
          skeyb(i)  = ' '
          do 30 j = 1, maxdoc
             doc(j,i) = ' '
 30       continue
          do 40 j = 1, mtitle
             titles(j,i) = ' '
 40       continue
          qweigh(i) = zero
          rweigh(i) = zero
          qwin1(i)  = zero
          qwin2(i)  = zero
          rwin1(i)  = zero
          rwin2(i)  = zero
          qmin(i)   = zero
          qmax(i)   = zero
          rmin(i)   = zero
          rmax(i)   = zero
          nqfit(i)  = 0
          nqpts(i)  = 0
          nrpts(i)  = 0
          sqwin(i)  = 'hanning'
          srwin(i)  = 'hanning'
          ifft(i)   = 1
          jffphs(i) = 0
          q1st(i)   = zero
          qlast(i)  = zero
          sigdtr(i) = zero
          sigdtk(i) = zero
          sigdtq(i) = zero
          sigwgt(i) = zero
          weight(i) = zero
          rfactr(i) = zero
          xinfo(i)  = zero
          ndoc(i)   = 0
          nkey(i)   = 0
          nkeyb(i)  = 0
          nchi(i)   = 0
          bkgfit(i) = .false.
          bkgdat(i) = .false.
          nbkg(i)   = 0
          datain(i) = .false.
          rbkg(i)   = zero
          do 70 j = 1, mtknot
             qknot(j,i) = zero
 70       continue
          chi2dt(i) = zero
          do 100 j = 1, maxpts
             chifit(j,i) = zero
             qwindo(j,i) = zero
             rwindo(j,i) = zero
             bkgq(j,i) = zero
             chiq(j,i) = zero
             thiq(j,i) = zero
 100      continue
          do 200 j = 0, mdpths
             jdtpth(j,i) = 0
             jdtusr(j,i) = 0
 200      continue
          do 240 j = 1, mlocal
             icdloc(1,j,i) = 0
 240      continue
 300   continue

c
c  loop over mvarys
       do 350 i = 1, mvarys
          xguess(i) = zero
          xfinal(i) = zero
          delta(i) = zero
          do 320 j = 1, mvarys
             correl(j,i) = zero
 320      continue
 350   continue

c
       do 500 i = 1, mfffil
          feffil(i) = ' '
          do 410 j =  1, mffttl
             fefttl(j,i) = '  '
 410      continue
          degpth(i) = one
          refpth(i) = zero
          rwgpth(i) = zero
          nlgpth(i) = 0
          iffrec(i) = 0
          do 420 j = 1, mffpts
             qfeff(j,i)  = zero
             theamp(j,i) = zero
             thepha(j,i) = zero
             realp(j,i)  = zero
             xlamb(j,i)  = zero
 420      continue
          do 460 j = 0, maxleg
             izpth(j,i)  = 0
             iptpth(j,i) = 0
             do 440 k = 1, 3
                ratpth(k,j,i) = zero
 440         continue
 460      continue
 500   continue

       do 700 i = 1, maxval
          values(i) = zero
          vnames(i) = ' '
          icdval(1,i) = 0
 700   continue
       do 750 i = 1, mpaths
          pthlab(i) = ' '
          jpthff(i) = 0
          do 720 j = 1, mpthpr
             icdpar(1,j,i) = 0
 720      continue
 750   continue
c
       do 780 i = 1, mpthpr
          defalt(i) = zero
 780   continue
       defalt(jps02) = one
       parnam(jps02) = 's02'
       parnam(jpe0)  = 'e0'
       parnam(jpei)  = 'ei'
       parnam(jpdpha)= 'dphase'
       parnam(jpdelr)= 'delr'
       parnam(jpsig2)= 'sigma2'
       parnam(jp3rd) = 'third'
       parnam(jp4th) = 'fourth'
       do 800 i = 1, mconst
          consts(i) = zero
 800   continue

       do 900 i = 1,   lwfft
          wfftc(i) = zero
 900   continue
c
c end subroutine fitint
       end

