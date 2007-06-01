c{data.h -*-fortran-*-
c  data and fitting numbers in feffit
       double precision chiq(maxpts,mdata)
       double precision thiq(maxpts,mdata),thiqr(maxpts,mdata)
       double precision qwindo(maxpts,mdata), rwindo(maxpts,mdata)
       double precision q1st(mdata), qlast(mdata)
       double precision chifit(maxpts, mdata), xnidp
       double precision sigdtr(mdata),sigdtk(mdata),sigdtq(mdata)
       double precision xinfo(mdata),chi2dt(mdata),rfactr(mdata)
       double precision sigwgt(mdata),weight(mdata)
       integer  ndoc(mdata), nkey(mdata), nchi(mdata), ndata
       integer  inform, nkeyb(mdata)
       common /data/  q1st, qlast, thiq, thiqr, chiq, chifit,
     $      qwindo, rwindo, sigdtr, sigdtk, sigdtq, sigwgt,
     $      weight, chi2dt, rfactr, xinfo,
     $      xnidp, ndoc, nkey, nchi, ndata, inform, nkeyb
c data.h}
