c{autcom.f -*-fortran-*-
       implicit none
c  parameters:
       integer maxpts, maxdoc, korder, maxnot, mtknot, mdmfft
       double precision    zero, one, pi, qgrid, etok
       parameter(maxpts  = 2048,  maxdoc = 20,   korder = 4 )
       parameter(maxnot  = 50,     mtknot = maxnot + korder  )
       parameter(mdmfft  = 4*maxpts + 15,  zero = 0.d0, one = 1.d0)
       parameter(pi      = 3.14159 26535 89793d0)
       parameter(qgrid   = 0.05d0 , etok   = 0.26246 82917d0)
c  character strings:
       character*128  xmudoc(maxdoc), thedoc(maxdoc)
       character*128   xmuf, theorf, chif, commnt, versn, winstr
       character*10   skeyth, skeyxm, frminp, frmout, asccmt*2
       common /char/  xmuf, skeyxm, theorf, skeyth, chif, commnt,
     $                frminp, frmout, versn, xmudoc, thedoc, asccmt, 
     $  winstr
c  data/background spline:
       integer nxmu, imucol, ntheor, nsplin, nkeyxm, nkeyth
       double precision energy(maxpts), xmudat(maxpts), windo(maxpts)
       double precision spline(maxpts), eknot(mtknot)
       double precision chiq(maxpts),  thiq(maxpts), thifit(maxpts)
       common /data/  nkeyxm, nkeyth, nxmu, ntheor, nsplin, imucol,
     $      energy, xmudat, spline, eknot, chiq, thiq, thifit, windo
c
c  pre-edge information:
       logical           eefind, stfind
       double precision  ee, predg1, predg2, slopre
       double precision  bpre, enor1, enor2, step, cnorm(3)
       integer   nnorm, nterp
       common /edge/  eefind, stfind, nnorm, nterp, ee, predg1, predg2,
     $                slopre, bpre, enor1, enor2, step, cnorm
c
c fast-fourier transform:
       double precision   wfftc(mdmfft), qweigh
       double precision   windo1, windo2, rbkg, r1st, qmin, qmax
       integer        iwindo, mftfit, nqpts, nrpts
       common /fft/   iwindo, mftfit, nqpts, nrpts, qweigh,
     $      qmin, qmax, windo1, windo2, rbkg, r1st, wfftc
c
c  input/output:
       integer       iprint, mdocxx, iodot, irecl
       logical       bkgxmu, bkgchi, bkgrsp, thechi, preout, eshout
       logical       thersp, chirsp, gvknot, nrmout
       logical       vaxflg, macflg, dosflg, unxflg
       common /ino/  mdocxx, iprint,  iodot, irecl, preout, nrmout, 
     $      eshout, bkgxmu, bkgchi, bkgrsp, thechi, thersp, chirsp, 
     $      gvknot, vaxflg, macflg, dosflg, unxflg
c
c  flags:
       logical       theory, thefix, eevary, funnrm, final
       double precision  usrtol, emin, emax, e0shft
       double precision  spstep, spfac, theamp, thessq
       integer       nrbkg, nr1st, nvarys, mfit
       common /flags/  theory, thefix, eevary, funnrm, final
       common /fit/  nrbkg, nr1st, emin, emax, e0shft, theamp,
     $             thessq, usrtol, nvarys,mfit, spstep, spfac

c# autcom.f}
