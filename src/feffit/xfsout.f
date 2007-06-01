       subroutine xfsout(chiq,chiqr,format,vax,filchi,filrsp,filenv,
     $      cmpchi,chiflg,rspflg,envflg,irecl,
     $      iexist,skychi,skyrsp,skyenv,cmt, ndocx,
     $      qwind, qweigh, rwind, rweigh,
     $      wfftc, mfft, rlast, qoutlo, qouthi, pcflg, qfeff, phafef,
     $      mfeff, qgrid, ndoc, doc)
c
c      given chi(k) and a whole lot of input parameters, this will
c      write output files with chi(k), chi(r), and backtransformed
c      chi(k).  for peculiar historical reasons, i will use the
c      convention that env and p refer to the two arrays for
c      backtransformed chi(k), and chi and q to the two arrays
c      for unfiltered chi(k).  the letter k is taken to be ambiguous
c      here (and a fortran integer) so it will be completely avoided.
c
c      copyright 1993 university of washington         matt newville
c
c  inputs:
c    chiq     array of unfiltered chi(k) data, on grid
c               such that chiq(1) = chi(k=0.)
c    filchi   name of output raw-k-space file to write
c    filrsp   name of output r-space file to write
c    filenv   name of output filtered-k-space file to write
c    format   format of output files (uwxafs of ascii)
c    vax      logical flag for writing binary data in vax format
c    chiflg   logical flag for writing raw-k-space data
c    rspflg   logical flag for writing r-space data
c    envflg   logical flag for writing filtered-k-space data
c    ndoc     number of document lines to write out
c    doc      documents to write out
c    qoutlo   lowest value in raw-k-space to write out data
c    qouthi   highest value in raw-k-space to write out data
c    qgrid    k-grid spacing for writing out data and fft
c    qweigh   k-weight to use for fft
c    qwin1    window parameter #1 for  k->r  ft
c    qwin2    window parameter #2 for  k->r  ft
c    wfftc    work array for fft (initialized with cffti using mfft )
c    mfft     number of points to use in fft ( .le.2048 )
c    rlast    highest r value to write out
c    iexist   integer flag for rewriting data to a uwexafs file
c  outputs:
c    skychi   output skey of chi(k) file (raw-k-space)
c    skyrsp   output skey of chi(r) file (r-space)
c    skyenv   output skey of env(p) file (filtered-k-space)
c
c    note mfft must be less than or equal to 2048
c
ccc       implicit none
       integer  maxpts, i, nqout, nqouth, nqoutl, nkyout, nrout,ndocx
       double precision qouthi, qoutlo, qsmall, qgrid
       double precision rsmall, rgrid, rlast
       integer  nfft, iexist, ndoc,  mfft, mfeff
       double precision  zero, pi
       parameter (maxpts = 2048, mxmpts=2048)
       parameter (zero = 0.d0,  pi = 3.141592653589793d0)
       character*128 filchi, filrsp, filenv, cmt*2
       character*100 doc(*)
       character*5   skychi, skyrsp, skyenv, format*10, type*10
       double precision  chiq(mfft), chiqr(mfft), wfftc(4*mfft+15)
       double precision  rwind(mfft), qwind(mfft), rweigh, qweigh
       double precision  xdata(maxpts), yreal(maxpts), yimag(maxpts)
       double precision  yphas(maxpts), yampl(maxpts)
       double precision  qfeff(mfeff), phafef(mfeff)
       complex*16        cchiq(mxmpts), cchir(mxmpts), coni
       parameter  (coni = (0.d0,1.d0))
       logical      vax, chiflg, rspflg, envflg, cmpchi, pcflg
c
c   initialize
       do 20 i = 1, maxpts
          xdata(i) = zero
          yreal(i) = zero
          yimag(i) = zero
          yampl(i) = zero
          yphas(i) = zero
          cchiq(i) = cmplx(zero, zero)
          cchir(i) = cmplx(zero, zero)
 20    continue

c check that mfft .le. maxpts
       nfft = maxpts
       rgrid  = pi / ( nfft * qgrid)
       rsmall = rgrid / 100.0
       qsmall = qgrid / 100.0
       nqoutl   = int( (qoutlo + qsmall) / qgrid )
       if (nqoutl.lt.0) nqoutl = 0
       nqouth   = int( (qouthi + qsmall) / qgrid )
       nqout  = nqouth - nqoutl + 1
       nrout  = int( (rlast + rsmall) / rgrid )
c
c   construct chi(k) on q range [qoutlo, qouthi]
       do 200 i = 1, nqout
          xdata(i) = qoutlo + (i-1)*qgrid
          yreal(i) = chiq(nqoutl + i)
 200   continue
c
c  k - space
       if (chiflg) then
          type   = 'chi'
          if (cmpchi) then
             call smcase(format,'u')
             if (format(1:2).ne.'uw') type   = 'env'
             do 240 i = 1, nqout
                yimag(i) = chiqr(nqoutl + i)
 240         continue
          end if
          nkyout = 0
c   write out chi(q) on q range [qoutlo, qouthi]
cc          print*, 'xfsout: outdat chi: ', filchi(1:20), ':'
          call outdat(type, format, filchi, vax,
     $         cmt, skychi, nkyout, irecl, ndoc, ndocx, doc,
     $         nqout, xdata, yreal, yimag, yampl, yphas, iexist)
          do 320 i = 1, nqout + 10
             xdata(i) = zero
             yreal(i) = zero
             yimag(i) = zero
             yampl(i) = zero
             yphas(i) = zero
 320       continue
       end if
c
c  construct chi(r)
c  construct complex chi(k)
c  take fft of complex chi(k) to get cchir
       if (rspflg.or.envflg) then
          if (pcflg) then
             ipos = 1
             do 400 i = 1, nfft
                q = (i-1) * qgrid
                call lintrp(qfeff, phafef, mfeff, q, ipos,  pha)
                cchiq(i) = cmplx(chiq(i), zero) * exp(-coni * pha)
 400         continue
          else
             do 430 i = 1, nfft
                cchiq(i) = cmplx(chiq(i), zero)
 430         continue
          end if
c          do ix = 20, 30
c             print*, nfft, ix, ix*qgrid, qwind(ix), cchiq(ix)
c          end do
          call xafsft(nfft, cchiq, qwind, qgrid, qweigh,
     $         wfftc, 1, cchir)
       end if
c
c write chi(r) on r range [0.,rlast]
       if (rspflg) then
          do 500 i = 1, nrout
             xdata(i) = (i-1)*rgrid
             yreal(i) = dble ( cchir(i))
             yimag(i) = dimag( cchir(i))
             yampl(i) = zero
             yphas(i) = zero
 500      continue
          type   = 'rsp'
          nkyout = 0
cc          print*, 'xfsout: outdat rsp: ', filrsp(1:20), ':'
          call outdat(type, format, filrsp, vax,
     $         cmt, skyrsp, nkyout, irecl, ndoc, ndocx, doc,
     $         nrout, xdata, yreal, yimag, yampl, yphas, iexist)
       end if
c
c construct env(k) on q range [qoutlo, qouthi]
       if (envflg) then
c  take back-fft of complex chir to get crhiq
          call xafsft(nfft, cchir, rwind, rgrid, rweigh, wfftc,
     $         -1, cchiq)
          do 800 i = 1, nqout
             xdata(i) = qoutlo + (i-1)*qgrid
             yreal(i) = dble ( cchiq(i+nqoutl))
             yimag(i) = dimag( cchiq(i+nqoutl))
             yampl(i) = zero
             yphas(i) = zero
 800      continue
          type   = 'env'
          nkyout = 0
cc          print*, 'xfsout: outdat env: ', filenv(1:20), ':'
          call outdat(type, format, filenv, vax,
     $         cmt, skyenv, nkyout, irecl, ndoc, ndocx, doc,
     $         nqout, xdata, yreal, yimag, yampl, yphas, iexist)
       end if
       return
c     end subroutine xfsout
       end
