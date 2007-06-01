c{math.h:  -*-fortran-*-
c numbers and integer codes for math expressions in feffit
       double precision  defalt(mpthpr), consts(mconst)
       double precision  values(maxval), delval(maxval)
       integer  icdpar(micode,mpthpr,mpaths)
       integer  icdval(micode, maxval), jpthff(mpaths)
       integer  icdloc(micode, mlocal, mdata), ixlocl
       parameter(ixlocl = 16384)
       integer  jdtpth(0:mdpths,mdata), jdtusr(0:mdpths,mdata)
       common /math_i/ icdpar, icdval, icdloc, jdtpth, jdtusr, jpthff
       common /math_d/ defalt, consts, values, delval
c math.h}
