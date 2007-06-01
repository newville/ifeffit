c{bkg.h -*-fortran-*-
c   background removal parameters in feffit...
       integer   mtknot, korder, nbkg(mdata)
       parameter (mtknot = 30, korder = 4 )
       logical  bkgfit(mdata), bkgout, bkgdat(mdata)
       double precision qknot(mtknot,mdata)
       double precision rbkg(mdata), bkgq(maxpts,mdata)
       common /bkg_l/ bkgfit, bkgdat, bkgout, nbkg
       common /bkg_d/ qknot, rbkg, bkgq
c bkg.h}
