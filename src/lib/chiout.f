       subroutine chiout(chiq, filksp, filrsp, format, vax, ksp, rsp,
     $      irecl, ndoc, doc, qlo, qhi, qgrid, qweigh, windo,
     $      wfftc, mfft, rlast, comm, mdocx, iexist, skychi, skyrsp)
c
c      this will write out chi(k) and chi(r) to output files
c
c      copyright 1993 university of washington         matt newville
c
c  inputs: 
c    chiq     array of chi(k) data, on with chiq(1) = chi(k=0.)
c    filksp   name of output k-space file to write
c    filrsp   name of output r-space file to write
c    format   format of output files (uwexafs of ascii)
c    vax      logical flag for writing binary data in vax format
c    ksp      logical flag for writing data to k-space
c    rsp      logical flag for writing data to r-space
c    ndoc     number of document lines to write out
c    doc      documents to write out
c    qlo      lowest value in k-space to write out data
c    qhi      highest value in k-space to write out data
c    qgrid    k- grid spacing for writing out data and fft
c    qweigh   k-weight to use for fft
c    windo    window array
c    windo2   window parameter #2
c    wfftc    work array for fft (initialized with cffti using mfft )
c    mfft     number of points to use in fft ( .le.2048 )
c    rlast    highest r value to write out  
c    iexist   integer flag for rewriting data to a uwexafs file
c  outputs:
c    skychi   output skey of chi(k) file
c    skyrsp   output skey of chi(r) file
c
c note mfft must be less than or equal to 2048
       implicit none
       integer   i, nfft, mdocx, maxpts, mfft,irecl
       double precision zero, pi
       parameter (maxpts = 4096)
       parameter (zero = 0.d0, pi = 3.14159 26535 89793d0  )
       character*(*) filksp, filrsp, format, doc, skychi, skyrsp
       character*(*) type*10, comm
       double precision chiq(*), wfftc(*), windo(*)
       double precision xdata(maxpts), yreal(maxpts), yimag(maxpts)
       double precision yphas(maxpts), yampl(maxpts)
       double precision qweigh, qgrid, qlo, qhi, rlast, rgrid
       double precision qsmall, rsmall
       integer   ndoc, nkyout, iexist, nrout, nqout, nqlo, nqhi
       complex*16       cchiq(maxpts), chir(maxpts)
       logical       vax, ksp, rsp
c
c   initialize, calculate assorted useful indices
        mdocx  = 0
        do 20 i = 1, maxpts
              xdata(i) = zero
              yreal(i) = zero
              yimag(i) = zero
              yampl(i) = zero
              yphas(i) = zero
              cchiq(i) = cmplx(zero, zero)
              chir(i)  = cmplx(zero, zero)
  20    continue
c check that mfft .le. maxpts
        nfft = mfft 
        if (nfft.gt.maxpts) nfft = maxpts
        rgrid  = pi / ( nfft * qgrid) 
        rsmall = rgrid / 100.0
        qsmall = qgrid / 100.0
        nqlo   = int( (qlo + qsmall) / qgrid ) 
        if (nqlo.lt.0) nqlo = 0
        nqhi   = int( (qhi + qsmall) / qgrid ) 
        nqout  = nqhi - nqlo + 1
        nrout  = int( (rlast + rsmall) / rgrid ) 
c
c   construct chi(k) on q range [qlo, qhi]
        do 300 i = 1, nqout
              xdata(i) = qlo + (i-1)*qgrid
              yreal(i) = chiq(nqlo + i)
              yimag(i) = zero
 300   continue
c
c  k - space
       if (ksp) then
           type   = 'chi'
           skychi = ' '
           nkyout = 0 
c   write out chi(k) on q range [qlo, qhi]
           call outdat(type, format, filksp, vax, comm, skychi, nkyout,
     $          irecl, ndoc, mdocx, doc, nqout, xdata,
     $          yreal, yimag, yampl, yphas, iexist)
       end if
c
c  r - space
c  
       if (rsp) then
c  construct complex chi(k)
           do 400 i = 1, nfft
                    cchiq(i) = cmplx(chiq(i), zero)
 400       continue
c  take fft of complex chi(k) to get chir
           call xafsft(nfft, cchiq, windo, qgrid, qweigh,
     $          wfftc, 1, chir)
           do 500 i = 1, nrout
                  xdata(i) = (i-1)*rgrid
                  yreal(i) = dble( chir(i) )
                  yimag(i) = dimag( chir(i) )
                  yampl(i) = zero
                  yphas(i) = zero
 500       continue
           type   = 'rsp'
           skyrsp = ' '
           nkyout = 0 
c  write out chi(r) on r range [0.,rlast]
           call outdat(type, format, filrsp, vax, comm, skyrsp, nkyout,
     $          irecl, ndoc, mdocx, doc, nrout, xdata,
     $          yreal, yimag, yampl, yphas, iexist)
       end if
c
       return
c  end subroutine chiout
       end
