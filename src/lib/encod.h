c{ifencod.h -*-fortran-*-
       integer   jscale, jconst, jlocal
c
c  note:  make sure jscale > maxarr  and jconst-jscale > maxsca
c  icode map:
c    1         -> jscale:  named arrays      1     -> 2**20
c    1+jscale  -> jconst:  named scalars     2**20 -> 2**24
c    1+jconst  -> 2**31 :  constant numbers
       parameter(jscale= 2**20, jconst= 8*jscale)
c
       integer ileft, iright, icomma
       integer iexp, ilog, ilog10, isqrt, isin, icos, itan, iabs
       integer iasin, iacos, iatan, isinh, icosh, itanh, icoth
       integer iadd, isub, imul, idiv, iy2x, ineg
       integer jadd, jsub, jmin, jmax, jdebye, jeins
       integer jpenl1, jpenl2, jpenl3
       integer jceil, jfloor, jvsum, jvprod, jnpts, jderiv, jsmoo
       integer jterpl, jterpq, jterps, jterpa, jrebin
       integer jndarr, j1sarr, j0sarr, jrngar, jasign
       integer jgamma, jlgamm, jerf, jerfc, jerfcx
       integer jlconv, jgconv, jkktf, jkktr, jfftf, jfftr
       integer jjoina, jslica, jnofxa
       parameter(ileft=  -6, iright=  -7, icomma= -8)
c 1-component math functions
c iop range: -1000 to -3000
       parameter(iexp= -1010, ilog= -1012, ilog10= -1013,
     $      isqrt = -1015,
     $      isin  = -1023,   icos = -1024,  itan= -1025,
     $      iasin = -1033,  iacos = -1034, iatan= -1035,
     $      isinh = -1043,  icosh = -1044, itanh= -1045, 
     $      iabs  = -1101,  ineg  = -1102, icoth= -1055, 
     $      jderiv= -1210,  jsmoo = -1220, jasign=-1230)
       parameter(jgamma=-2005, jlgamm=-2006,
     $      jerf=-2010, jerfc=-2011, jerfcx=-2012)

c 2-component math functions
c iop range: -5000 to -6000
       parameter(iadd= -5000, isub= -5001, imul= -5002,
     $      idiv= -5003, iy2x= -5004) 
       parameter( jadd= -7101, jsub= -7102,
     $      jmin= -7103, jmax= -7104)

c special math function -- call their own routines
c iop range: -9000 to -10000
       parameter(jdebye=-9020, jeins=-9021,  jrngar=-9030 ,
     $      jndarr=-9031,  j0sarr=-9032,  j1sarr=-9033,
     $      jjoina=-9041,  jslica=-9042,  jnofxa=-9043,
     $      jterpl=-9120,  jterpq=-9121,  jterps=-9122,
     $      jterpa=-9123,  jrebin=-9125)
       parameter(jlconv=-9201,  jgconv=-9202,
     $      jkktf =-9210,  jkktr =-9211, 
     $      jfftf =-9213,  jfftr =-9214)
       parameter(jpenl1 = -9221,jpenl2 = -9222, jpenl3 = -9223)

       integer jxgaus, jxlore, jxvoit, jxcube, jxstep
c iop range: -9000 to -10000
       parameter(jxgaus = -9301, jxlore = -9302, jxvoit = -9303,
     $           jxcube = -9304, jxstep = -9305)

c 1 component vector math: 1 array argument, returns scalar.
c iop range: -30000 to -32000
       parameter (jceil=-30001, jfloor=-30002, jnpts=-30003, 
     $      jvsum=-30004, jvprod=-30005)

       double precision expmax
       parameter (expmax= 85.d0)
c}

