c{pthpar.h -*-fortran-*-
       double precision  defalt(mpthpr), param(mpthpr)
       integer  icdpar(micode,mpthpr,mpaths)
       integer  jusedg(mpaths), jpthff(mpaths)
       integer  jdtpth(0:mpaths), jdtusr(0:mpaths)
       integer  inpthx, itfeff
       integer  jfps02, jfpe0, jfpei, jfpdr, jfpss2, jfp3rd, jfp4th
       integer  jfpkar, jfppha, jfpaar, jfppar, jfppth, jfpdeg
       parameter(jfps02 =  1, jfpe0  =  2, jfpei  =  3, jfpdr  =  4)
       parameter(jfpss2 =  5, jfp3rd =  6, jfp4th =  7, jfppha =  8)
       parameter(jfpkar = 10, jfpaar = 11, jfppar = 12, jfppth = 13)
       parameter(jfpdeg =  9)
       character*128  pthlab(mpaths)
       common /xptin/ icdpar, jdtpth, jdtusr, jpthff,
     $      jusedg, inpthx, itfeff
       common /xptlg/ defalt, param
       common /xptch/ pthlab
c}
