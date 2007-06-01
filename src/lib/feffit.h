c{feffit.h -*-fortran-*-
       double precision xguess(mvarys), xfinal(mvarys), delta(mvarys)
       double precision correl(mvarys, mvarys), chisqr, usrtol
       double precision q1st(mdata), qlast(mdata)
       double precision thifit(maxpts), chifit(maxpts)
       double precision chiq(maxpts,mdata),thiq(maxpts,mdata)
       double precision thiqr(maxpts,mdata), xnidp
       double precision qwindo(maxpts,mdata), rwindo(maxpts,mdata)
       double precision xinfo(mdata),  rfactr(mdata), rfact_total
       double precision rweigh(mdata), rmin(mdata), rmax(mdata)
       double precision qweigh(mdata), qmin(mdata), qmax(mdata)
       double precision dq1(mdata), dq2(mdata)
       double precision sigwgt
       double precision sigdtr, sigdtk, sigdtq
       double precision qknot(mtknot,mdata),rbkg_f(mdata)
       double precision bkgq(maxpts,mdata)

       character*128    fit_macro*32, fit_m_arg
       character*128    cfmin_arr, cfmin_pre, cfmin_err
       character*128    restraint(max_restraint,mdata)
       integer   nrestraint(mdata)

       logical   bkgfit(mdata), bkgdat(mdata), usewgt, final
       integer   nqfit(mdata), nqpts(mdata), nrpts(mdata), nqvals(mdata)
       integer   iqwin(mdata), irwin(mdata), ifft(mdata), jffphs(mdata)
       integer   nchi(mdata), nbkg(mdata), nfit(mdata)
       integer   nfdats, inform, nvarys, mfit , nmacxx_save
       integer   ifxvar, numvar, nvuser, ncarr, nconst
       integer   nxscal, nxarr, iulist(mpaths, mdata)
       integer   ifitx1, ifitx2, ifvar, iferr, ierbar, itera, ifit_mac

c  for multiple-k-weights in fit
       integer   nqwfs(mdata), mqwfs
       parameter (mqwfs = 5)
       double precision qwfs(mqwfs,mdata), weight(mqwfs,mdata)

       common /fxtvr/ xguess, xfinal, delta, correl, chisqr, usrtol,
     $      chifit, q1st, qlast, chiq,thiq,thiqr, xnidp, qwindo, 
     $      rwindo, sigdtr, sigdtk,sigdtq, xinfo, 
     $      rfactr, rfact_total, sigwgt, weight, 
     $      rmin, rmax, rweigh, qmin, qmax, qweigh, 
     $      dq1, dq2, thifit, qknot, rbkg_f, bkgq,  qwfs

       common /fxtlg/  final, bkgfit, bkgdat, usewgt

       common /fxtin/  nqfit, nqpts, itera, ifit_mac,
     $      nrpts, iqwin, irwin, ifft, jffphs, nchi, nbkg, nfdats, 
     $      inform, nvarys, mfit, ifvar, iferr, ifxvar, numvar, 
     $      nvuser, nxscal, nxarr, ncarr,  nconst, iulist, 
     $      ierbar, ifitx1, ifitx2, nqvals, nfit, nmacxx_save,
     $      nqwfs, nrestraint

       common /fxtch/  fit_macro, fit_m_arg

       common /cfmin/ cfmin_arr, cfmin_pre, cfmin_err, restraint
c}
