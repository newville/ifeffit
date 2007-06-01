c{spline.h -*-fortran-*-
       integer          nsplin, nautbk, nxmu, nr1st, nrbkg, nclamp
       logical          theory, eevary, thefix, funnrm, clamp(2)
       double precision de0, e0, emin, emax, chie(maxpts), rbkg,r1st
       double precision endat(maxpts), xmudat(maxpts)
       double precision eknot(mtknot), thessq, thebkg, step, splqw
       double precision splwin(maxpts), splfit(maxpts)
       double precision spldat(maxpts), sclamp(2)
cc       double precision cld_wgt(maxpts), cld_dat(maxpts), cld_f2(maxpts)
       common /xbkin/   nsplin,nautbk, nxmu, nr1st, nrbkg, nclamp
       common /xbklg/   funnrm, thefix, theory, eevary,clamp
       common /xbkvr/   rbkg, r1st, thessq, thebkg, step, 
     $      e0, de0, emin, emax, endat, xmudat, chie, 
     $      eknot, splqw, splwin, splfit, spldat, sclamp
c}

