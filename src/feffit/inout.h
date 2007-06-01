c{inout.h -*-fortran-*-
c  miscellaneous input/output stuff in feffit
       double precision  rlast, cormin, tranq,rwght1, rwght2
       integer iprint, mdocxx, irecl(mdata)
       logical allout, kspcmp, kspout, rspout, qspout, degflg
       logical datain(mdata), rm2flg, dphflg
       logical noout, nofit, final, vaxflg, dosflg, macflg
       logical pcout, prmout, chkdat
       common /inout/ rlast,cormin,tranq,rwght1,rwght2,iprint,mdocxx,
     $      irecl, final,allout, kspcmp,kspout,rspout,qspout,
     $      degflg, prmout, pcout, chkdat,
     $      datain, noout, nofit,vaxflg,dosflg,macflg,rm2flg,dphflg
c inout.h}
