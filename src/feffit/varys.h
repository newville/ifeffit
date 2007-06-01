c{varys.h -*-fortran-*-
c  values for variables of fit and error analysis in feffit
c (nmathx = number of user defined math expressions)
       double precision xguess(mvarys), xfinal(mvarys), delta(mvarys)
       double precision correl(mvarys, mvarys), chisqr, usrtol
       integer     ifxvar, numvar, nvuser, nmathx, nconst
       integer     ierbar, nerstp
       common /varys/ xguess, xfinal, delta, correl, chisqr,
     $                usrtol, numvar, nvuser, ifxvar,
     $                ierbar, nerstp, nmathx, nconst
c varys.h}
