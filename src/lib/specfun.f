CC
CC  various special functions, taken from netlib's specfun package
CC
       double precision function erf_xx(arg,jint)
C------------------------------------------------------------------
C
C This packet evaluates  erf(x),  erfc(x),  and  exp(x*x)*erfc(x)
C   for a real argument  x.  It contains three FUNCTION type
C   subprograms: ERF, ERFC, and ERFCX (or DERF, DERFC, and DERFCX),
C   and one SUBROUTINE type subprogram, ERF_XX.  The calling
C   statements for the primary entries are:
C
C                   Y=ERF(X)     (or   Y=DERF(X)),
C
C                   Y=ERFC(X)    (or   Y=DERFC(X)),
C   and
C                   Y=ERFCX(X)   (or   Y=DERFCX(X)).
C
C   The routine  ERF_XX  is intended for internal packet use only,
C   all computations within the packet being concentrated in this
C   routine.  The function subprograms invoke  ERF_XX  with the
C   statement
C
C          CALL ERF_XX(ARG,RESULT,JINT)
C
C   where the parameter usage is as follows
C
C      Function                     Parameters for ERF_XX
C       call              ARG                  Result          JINT
C
C     ERF(ARG)      ANY REAL ARGUMENT         ERF(ARG)          0
C     ERFC(ARG)     ABS(ARG) .LT. XBIG        ERFC(ARG)         1
C     ERFCX(ARG)    XNEG .LT. ARG .LT. XMAX   ERFCX(ARG)        2
C
C   The main computation evaluates near-minimax approximations
C   from "Rational Chebyshev approximations for the error function"
C   by W. J. Cody, Math. Comp., 1969, PP. 631-638.  This
C   transportable program uses rational functions that theoretically
C   approximate  erf(x)  and  erfc(x)  to at least 18 significant
C   decimal digits.  The accuracy achieved depends on the arithmetic
C   system, the compiler, the intrinsic functions, and proper
C   selection of the machine-dependent constants.
C
C*******************************************************************
C*******************************************************************
C
C Explanation of machine-dependent constants
C
C   XMIN   = the smallest positive floating-point number.
C   XINF   = the largest positive finite floating-point number.
C   XNEG   = the largest negative argument acceptable to ERFCX;
C            the negative of the solution to the equation
C            2*exp(x*x) = XINF.
C   XSMALL = argument below which erf(x) may be represented by
C            2*x/sqrt(pi)  and above which  x*x  will not underflow.
C            A conservative value is the largest machine number X
C            such that   1.0 + X = 1.0   to machine precision.
C   XBIG   = largest argument acceptable to ERFC;  solution to
C            the equation:  W(x) * (1-0.5/x**2) = XMIN,  where
C            W(x) = exp(-x*x)/[x*sqrt(pi)].
C   XHUGE  = argument above which  1.0 - 1/(2*x*x) = 1.0  to
C            machine precision.  A conservative value is
C            1/[2*sqrt(XSMALL)]
C   XMAX   = largest acceptable argument to ERFCX; the minimum
C            of XINF and 1/[sqrt(pi)*XMIN].
C
C*******************************************************************
C*******************************************************************
C
C Error returns
C
C  The program returns  ERFC = 0      for  ARG .GE. XBIG;
C
C                       ERFCX = XINF  for  ARG .LT. XNEG;
C      and
C                       ERFCX = 0     for  ARG .GE. XMAX.
C
C Intrinsic functions required are:
C
C     ABS, INT, EXP
C
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C  Latest modification: March 19, 1990
C
c------------------------------------------------------------------
       integer i,jint
       double precision arg,del,four,half,one, y, ysq, sixteen
       double precision sqrpi,two,thresh,x,xbig,xden,xhuge
       double precision xinf,xmax,xneg,xnum,xsmall, zero
       double precision a(5),b(4),c(9),d(8),p(6),q(5)
c------------------------------------------------------------------
c  mathematical constants
c------------------------------------------------------------------
       parameter(four=4.d0,one=1.d0,half=0.5d0,two=2.d0,zero=0.d0)
       parameter(sqrpi=5.6418958354775628695d-1,thresh=0.46875d0)
       parameter(sixten=16.0d0)
c------------------------------------------------------------------
c  machine-dependent constants
c------------------------------------------------------------------
       parameter(xinf =1.d50, xneg=-22.d0, xsmall=1.d-16)
       parameter(xbig =22.d0,  xhuge=6.d6, xmax=xinf)
c------------------------------------------------------------------
c  coefficients for approximation to  erf  in first interval
c------------------------------------------------------------------
       data a/3.16112374387056560d00,1.13864154151050156d02,
     1      3.77485237685302021d02,3.20937758913846947d03,
     2      1.85777706184603153d-1/
       data b/2.36012909523441209d01,2.44024637934444173d02,
     1      1.28261652607737228d03,2.84423683343917062d03/
c------------------------------------------------------------------
c  coefficients for approximation to  erfc  in second interval
c------------------------------------------------------------------
       data c/5.64188496988670089d-1,8.88314979438837594d0,
     1      6.61191906371416295d01,2.98635138197400131d02,
     2      8.81952221241769090d02,1.71204761263407058d03,
     3      2.05107837782607147d03,1.23033935479799725d03,
     4      2.15311535474403846d-8/
       data d/1.57449261107098347d01,1.17693950891312499d02,
     1      5.37181101862009858d02,1.62138957456669019d03,
     2      3.29079923573345963d03,4.36261909014324716d03,
     3      3.43936767414372164d03,1.23033935480374942d03/
c------------------------------------------------------------------
c      coefficients for approximation to  erfc  in third interval
c------------------------------------------------------------------
       data p/3.05326634961232344d-1,3.60344899949804439d-1,
     1      1.25781726111229246d-1,1.60837851487422766d-2,
     2      6.58749161529837803d-4,1.63153871373020978d-2/
       data q/2.56852019228982242d00,1.87295284992346047d00,
     1      5.27905102951428412d-1,6.05183413124413191d-2,
     2      2.33520497626869185d-3/
c------------------------------------------------------------------
       x = arg
       y = abs(x)
       if (y .le. thresh) then
c------------------------------------------------------------------
c  evaluate  erf  for  |x| <= 0.46875
c------------------------------------------------------------------
          ysq = zero
          if (y .gt. xsmall) ysq = y * y
          xnum = a(5)*ysq
          xden = ysq
          do 20 i = 1, 3
             xnum = (xnum + a(i)) * ysq
             xden = (xden + b(i)) * ysq
 20       continue
          erf_xx = x * (xnum + a(4)) / (xden + b(4))
          if (jint .ne. 0) erf_xx = one - erf_xx
          if (jint .eq. 2) erf_xx = exp(ysq) * erf_xx
          go to 800
c------------------------------------------------------------------
c  evaluate  erfc  for 0.46875 <= |x| <= 4.0
c------------------------------------------------------------------
       else if (y .le. four) then
          xnum = c(9)*y
          xden = y
          do 120 i = 1, 7
             xnum = (xnum + c(i)) * y
             xden = (xden + d(i)) * y
 120      continue
          erf_xx = (xnum + c(8)) / (xden + d(8))
          if (jint .ne. 2) then
             ysq = int(y*sixten)/sixten
             del = (y-ysq)*(y+ysq)
             erf_xx = exp(-ysq*ysq) * exp(-del) * erf_xx
          end if
c------------------------------------------------------------------
c  evaluate  erfc  for |x| > 4.0
c------------------------------------------------------------------
       else
          erf_xx = zero
          if (y .ge. xbig) then
             if ((jint .ne. 2) .or. (y .ge. xmax)) go to 300
             if (y .ge. xhuge) then
                erf_xx = sqrpi / y
                go to 300
             end if
          end if
          ysq = one / (y * y)
          xnum = p(6)*ysq
          xden = ysq
          do 240 i = 1, 4
             xnum = (xnum + p(i)) * ysq
             xden = (xden + q(i)) * ysq
 240      continue
          erf_xx = ysq *(xnum + p(5)) / (xden + q(5))
          erf_xx = (sqrpi -  erf_xx) / y
          if (jint .ne. 2) then
             ysq   = int(y*sixten)/sixten
             del   = (y-ysq)*(y+ysq)
             erf_xx = exp(-ysq*ysq) * exp(-del) * erf_xx
          end if
       end if
c------------------------------------------------------------------
c  fix up for negative argument, erf, etc.
c------------------------------------------------------------------
 300   if (jint .eq. 0) then
          erf_xx = (half - erf_xx) + half
          if (x .lt. zero) erf_xx = -erf_xx
       else if (jint .eq. 1) then
          if (x .lt. zero) erf_xx = two - erf_xx
       else
          if (x .lt. zero) then
             if (x .lt. xneg) then
                erf_xx = xinf
             else
                ysq = int(x*sixten)/sixten
                del = (x-ysq)*(x+ysq)
                y   = exp(ysq*ysq) * exp(del)
                erf_xx = (y+y) - erf_xx
             end if
          end if
       end if
 800   return
       end
       DOUBLE PRECISION FUNCTION DGAMMA(X)
C----------------------------------------------------------------------
C
C This routine calculates the GAMMA function for a real argument X.
C   Computation is based on an algorithm outlined in reference 1.
C   The program uses rational functions that approximate the GAMMA
C   function to at least 20 significant decimal digits.  Coefficients
C   for the approximation over the interval (1,2) are unpublished.
C   Those for the approximation for X .GE. 12 are from reference 2.
C   The accuracy achieved depends on the arithmetic system, the
C   compiler, the intrinsic functions, and proper selection of the
C   machine-dependent constants.
C
C
C*******************************************************************
C*******************************************************************
C
C Explanation of machine-dependent constants
C
C beta   - radix for the floating-point representation
C maxexp - the smallest positive power of beta that overflows
C XBIG   - the largest argument for which GAMMA(X) is representable
C          in the machine, i.e., the solution to the equation
C                  GAMMA(XBIG) = beta**maxexp
C XINF   - the largest machine representable floating-point number;
C          approximately beta**maxexp
C EPS    - the smallest positive floating-point number such that
C          1.0+EPS .GT. 1.0
C XMININ - the smallest positive floating-point number such that
C          1/XMININ is machine representable
C
C     Approximate values for some important machines are:
C
C                            beta       maxexp        XBIG
C
C CRAY-1         (S.P.)        2         8191        966.961
C Cyber 180/855
C   under NOS    (S.P.)        2         1070        177.803
C IEEE (IBM/XT,
C   SUN, etc.)   (S.P.)        2          128        35.040
C IEEE (IBM/XT,
C   SUN, etc.)   (D.P.)        2         1024        171.624
C IBM 3033       (D.P.)       16           63        57.574
C VAX D-Format   (D.P.)        2          127        34.844
C VAX G-Format   (D.P.)        2         1023        171.489
C
C                            XINF         EPS        XMININ
C
C CRAY-1         (S.P.)   5.45E+2465   7.11E-15    1.84E-2466
C Cyber 180/855
C   under NOS    (S.P.)   1.26E+322    3.55E-15    3.14E-294
C IEEE (IBM/XT,
C   SUN, etc.)   (S.P.)   3.40E+38     1.19E-7     1.18E-38
C IEEE (IBM/XT,
C   SUN, etc.)   (D.P.)   1.79D+308    2.22D-16    2.23D-308
C IBM 3033       (D.P.)   7.23D+75     2.22D-16    1.39D-76
C VAX D-Format   (D.P.)   1.70D+38     1.39D-17    5.88D-39
C VAX G-Format   (D.P.)   8.98D+307    1.11D-16    1.12D-308
C
C*******************************************************************
C*******************************************************************
C
C Error returns
C
C  The program returns the value XINF for singularities or
C     when overflow would occur.  The computation is believed
C     to be free of underflow and overflow.
C
C
C  Intrinsic functions required are:
C
C     INT, DBLE, EXP, LOG, REAL, SIN
C
C
C References: "An Overview of Software Development for Special
C              Functions", W. J. Cody, Lecture Notes in Mathematics,
C              506, Numerical Analysis Dundee, 1975, G. A. Watson
C              (ed.), Springer Verlag, Berlin, 1976.
C
C              Computer Approximations, Hart, Et. Al., Wiley and
C              sons, New York, 1968.
C
C  Latest modification: October 12, 1989
C
C  Authors: W. J. Cody and L. Stoltz
C           Applied Mathematics Division
C           Argonne National Laboratory
C           Argonne, IL 60439
C
C----------------------------------------------------------------------
      INTEGER I,N
      LOGICAL PARITY
      DOUBLE PRECISION 
     1    C,CONV,EPS,FACT,HALF,ONE,P,PI,Q,RES,SQRTPI,SUM,TWELVE,
     2    TWO,X,XBIG,XDEN,XINF,XMININ,XNUM,Y,Y1,YSQ,Z,ZERO
      DIMENSION C(7),P(8),Q(8)
C----------------------------------------------------------------------
C  Mathematical constants
C----------------------------------------------------------------------
      DATA ONE,HALF,TWELVE,TWO,ZERO/1.0D0,0.5D0,12.0D0,2.0D0,0.0D0/,
     1     SQRTPI/0.9189385332046727417803297D0/,
     2     PI/3.1415926535897932384626434D0/
C----------------------------------------------------------------------
C  Machine dependent parameters
C----------------------------------------------------------------------
      DATA XBIG,XMININ,EPS/171.624D0,2.23D-308,2.22D-16/,
     1     XINF/1.79D308/
C----------------------------------------------------------------------
C  Numerator and denominator coefficients for rational minimax
C     approximation over (1,2).
C----------------------------------------------------------------------
      DATA P/-1.71618513886549492533811D+0,2.47656508055759199108314D+1,
     1       -3.79804256470945635097577D+2,6.29331155312818442661052D+2,
     2       8.66966202790413211295064D+2,-3.14512729688483675254357D+4,
     3       -3.61444134186911729807069D+4,6.64561438202405440627855D+4/
      DATA Q/-3.08402300119738975254353D+1,3.15350626979604161529144D+2,
     1      -1.01515636749021914166146D+3,-3.10777167157231109440444D+3,
     2        2.25381184209801510330112D+4,4.75584627752788110767815D+3,
     3      -1.34659959864969306392456D+5,-1.15132259675553483497211D+5/
C----------------------------------------------------------------------
C  Coefficients for minimax approximation over (12, INF).
C----------------------------------------------------------------------
      DATA C/-1.910444077728D-03,8.4171387781295D-04,
     1     -5.952379913043012D-04,7.93650793500350248D-04,
     2     -2.777777777777681622553D-03,8.333333333333333331554247D-02,
     3      5.7083835261D-03/
C----------------------------------------------------------------------
C  Statement functions for conversion between integer and float
C----------------------------------------------------------------------
      CONV(I) = DBLE(I)
      PARITY = .FALSE.
      FACT = ONE
      N = 0
      Y = X
      IF (Y .LE. ZERO) THEN
C----------------------------------------------------------------------
C  Argument is negative
C----------------------------------------------------------------------
            Y = -X
            Y1 = AINT(Y)
            RES = Y - Y1
            IF (RES .NE. ZERO) THEN
                  IF (Y1 .NE. AINT(Y1*HALF)*TWO) PARITY = .TRUE.
                  FACT = -PI / SIN(PI*RES)
                  Y = Y + ONE
               ELSE
                  RES = XINF
                  GO TO 900
            END IF
      END IF
C----------------------------------------------------------------------
C  Argument is positive
C----------------------------------------------------------------------
      IF (Y .LT. EPS) THEN
C----------------------------------------------------------------------
C  Argument .LT. EPS
C----------------------------------------------------------------------
            IF (Y .GE. XMININ) THEN
                  RES = ONE / Y
               ELSE
                  RES = XINF
                  GO TO 900
            END IF
         ELSE IF (Y .LT. TWELVE) THEN
            Y1 = Y
            IF (Y .LT. ONE) THEN
C----------------------------------------------------------------------
C  0.0 .LT. argument .LT. 1.0
C----------------------------------------------------------------------
                  Z = Y
                  Y = Y + ONE
               ELSE
C----------------------------------------------------------------------
C  1.0 .LT. argument .LT. 12.0, reduce argument if necessary
C----------------------------------------------------------------------
                  N = INT(Y) - 1
                  Y = Y - CONV(N)
                  Z = Y - ONE
            END IF
C----------------------------------------------------------------------
C  Evaluate approximation for 1.0 .LT. argument .LT. 2.0
C----------------------------------------------------------------------
            XNUM = ZERO
            XDEN = ONE
            DO 260 I = 1, 8
               XNUM = (XNUM + P(I)) * Z
               XDEN = XDEN * Z + Q(I)
  260       CONTINUE
            RES = XNUM / XDEN + ONE
            IF (Y1 .LT. Y) THEN
C----------------------------------------------------------------------
C  Adjust result for case  0.0 .LT. argument .LT. 1.0
C----------------------------------------------------------------------
                  RES = RES / Y1
               ELSE IF (Y1 .GT. Y) THEN
C----------------------------------------------------------------------
C  Adjust result for case  2.0 .LT. argument .LT. 12.0
C----------------------------------------------------------------------
                  DO 290 I = 1, N
                     RES = RES * Y
                     Y = Y + ONE
  290             CONTINUE
            END IF
         ELSE
C----------------------------------------------------------------------
C  Evaluate for argument .GE. 12.0,
C----------------------------------------------------------------------
            IF (Y .LE. XBIG) THEN
                  YSQ = Y * Y
                  SUM = C(7)
                  DO 350 I = 1, 6
                     SUM = SUM / YSQ + C(I)
  350             CONTINUE
                  SUM = SUM/Y - Y + SQRTPI
                  SUM = SUM + (Y-HALF)*LOG(Y)
                  RES = EXP(SUM)
               ELSE
                  RES = XINF
                  GO TO 900
            END IF
      END IF
C----------------------------------------------------------------------
C  Final adjustments and return
C----------------------------------------------------------------------
      IF (PARITY) RES = -RES
      IF (FACT .NE. ONE) RES = FACT / RES
  900 DGAMMA = RES
      RETURN
C ---------- Last line of GAMMA ----------
      END
      DOUBLE PRECISION FUNCTION DLGAMA(X)
C----------------------------------------------------------------------
C
C This routine calculates the LOG(GAMMA) function for a positive real
C   argument X.  Computation is based on an algorithm outlined in
C   references 1 and 2.  The program uses rational functions that
C   theoretically approximate LOG(GAMMA) to at least 18 significant
C   decimal digits.  The approximation for X > 12 is from reference
C   3, while approximations for X < 12.0 are similar to those in
C   reference 1, but are unpublished.  The accuracy achieved depends
C   on the arithmetic system, the compiler, the intrinsic functions,
C   and proper selection of the machine-dependent constants.
C
C
C*********************************************************************
C*********************************************************************
C
C Explanation of machine-dependent constants
C
C beta   - radix for the floating-point representation
C maxexp - the smallest positive power of beta that overflows
C XBIG   - largest argument for which LN(GAMMA(X)) is representable
C          in the machine, i.e., the solution to the equation
C                  LN(GAMMA(XBIG)) = beta**maxexp
C XINF   - largest machine representable floating-point number;
C          approximately beta**maxexp.
C EPS    - The smallest positive floating-point number such that
C          1.0+EPS .GT. 1.0
C FRTBIG - Rough estimate of the fourth root of XBIG
C
C
C     Approximate values for some important machines are:
C
C                           beta      maxexp         XBIG
C
C CRAY-1        (S.P.)        2        8191       9.62E+2461
C Cyber 180/855
C   under NOS   (S.P.)        2        1070       1.72E+319
C IEEE (IBM/XT,
C   SUN, etc.)  (S.P.)        2         128       4.08E+36
C IEEE (IBM/XT,
C   SUN, etc.)  (D.P.)        2        1024       2.55D+305
C IBM 3033      (D.P.)       16          63       4.29D+73
C VAX D-Format  (D.P.)        2         127       2.05D+36
C VAX G-Format  (D.P.)        2        1023       1.28D+305
C
C
C                           XINF        EPS        FRTBIG
C
C CRAY-1        (S.P.)   5.45E+2465   7.11E-15    3.13E+615
C Cyber 180/855
C   under NOS   (S.P.)   1.26E+322    3.55E-15    6.44E+79
C IEEE (IBM/XT,
C   SUN, etc.)  (S.P.)   3.40E+38     1.19E-7     1.42E+9
C IEEE (IBM/XT,
C   SUN, etc.)  (D.P.)   1.79D+308    2.22D-16    2.25D+76
C IBM 3033      (D.P.)   7.23D+75     2.22D-16    2.56D+18
C VAX D-Format  (D.P.)   1.70D+38     1.39D-17    1.20D+9
C VAX G-Format  (D.P.)   8.98D+307    1.11D-16    1.89D+76
C
C**************************************************************
C**************************************************************
C
C Error returns
C
C  The program returns the value XINF for X .LE. 0.0 or when
C     overflow would occur.  The computation is believed to 
C     be free of underflow and overflow.
C
C
C Intrinsic functions required are:
C
C      LOG
C
C
C References:
C
C  1) W. J. Cody and K. E. Hillstrom, 'Chebyshev Approximations for
C     the Natural Logarithm of the Gamma Function,' Math. Comp. 21,
C     1967, pp. 198-203.
C
C  2) K. E. Hillstrom, ANL/AMD Program ANLC366S, DGAMMA/DLGAMA, May,
C     1969.
C 
C  3) Hart, Et. Al., Computer Approximations, Wiley and sons, New
C     York, 1968.
C
C
C  Authors: W. J. Cody and L. Stoltz
C           Argonne National Laboratory
C
C  Latest modification: June 16, 1988
C
C----------------------------------------------------------------------
      INTEGER I
      DOUBLE PRECISION
     1    C,CORR,D1,D2,D4,EPS,FRTBIG,FOUR,HALF,ONE,PNT68,P1,P2,P4,
     2    Q1,Q2,Q4,RES,SQRTPI,THRHAL,TWELVE,TWO,X,XBIG,XDEN,XINF,
     3    XM1,XM2,XM4,XNUM,Y,YSQ,ZERO
      DIMENSION C(7),P1(8),P2(8),P4(8),Q1(8),Q2(8),Q4(8)
C----------------------------------------------------------------------
C  Mathematical constants
C----------------------------------------------------------------------
      DATA ONE,HALF,TWELVE,ZERO/1.0D0,0.5D0,12.0D0,0.0D0/,
     1     FOUR,THRHAL,TWO,PNT68/4.0D0,1.5D0,2.0D0,0.6796875D0/,
     2     SQRTPI/0.9189385332046727417803297D0/
C----------------------------------------------------------------------
C  Machine dependent parameters
C----------------------------------------------------------------------
      DATA XBIG,XINF,EPS,FRTBIG/2.55D305,1.79D308,2.22D-16,2.25D76/
C----------------------------------------------------------------------
C  Numerator and denominator coefficients for rational minimax
C     approximation over (0.5,1.5).
C----------------------------------------------------------------------
      DATA D1/-5.772156649015328605195174D-1/
      DATA P1/4.945235359296727046734888D0,2.018112620856775083915565D2,
     1        2.290838373831346393026739D3,1.131967205903380828685045D4,
     2        2.855724635671635335736389D4,3.848496228443793359990269D4,
     3        2.637748787624195437963534D4,7.225813979700288197698961D3/
      DATA Q1/6.748212550303777196073036D1,1.113332393857199323513008D3,
     1        7.738757056935398733233834D3,2.763987074403340708898585D4,
     2        5.499310206226157329794414D4,6.161122180066002127833352D4,
     3        3.635127591501940507276287D4,8.785536302431013170870835D3/
C----------------------------------------------------------------------
C  Numerator and denominator coefficients for rational minimax
C     Approximation over (1.5,4.0).
C----------------------------------------------------------------------
      DATA D2/4.227843350984671393993777D-1/
      DATA P2/4.974607845568932035012064D0,5.424138599891070494101986D2,
     1        1.550693864978364947665077D4,1.847932904445632425417223D5,
     2        1.088204769468828767498470D6,3.338152967987029735917223D6,
     3        5.106661678927352456275255D6,3.074109054850539556250927D6/
      DATA Q2/1.830328399370592604055942D2,7.765049321445005871323047D3,
     1        1.331903827966074194402448D5,1.136705821321969608938755D6,
     2        5.267964117437946917577538D6,1.346701454311101692290052D7,
     3        1.782736530353274213975932D7,9.533095591844353613395747D6/
C----------------------------------------------------------------------
C  Numerator and denominator coefficients for rational minimax
C     Approximation over (4.0,12.0).
C----------------------------------------------------------------------
      DATA D4/1.791759469228055000094023D0/
      DATA P4/1.474502166059939948905062D4,2.426813369486704502836312D6,
     1        1.214755574045093227939592D8,2.663432449630976949898078D9,
     2      2.940378956634553899906876D10,1.702665737765398868392998D11,
     3      4.926125793377430887588120D11,5.606251856223951465078242D11/
      DATA Q4/2.690530175870899333379843D3,6.393885654300092398984238D5,
     2        4.135599930241388052042842D7,1.120872109616147941376570D9,
     3      1.488613728678813811542398D10,1.016803586272438228077304D11,
     4      3.417476345507377132798597D11,4.463158187419713286462081D11/
C----------------------------------------------------------------------
C  Coefficients for minimax approximation over (12, INF).
C----------------------------------------------------------------------
      DATA C/-1.910444077728D-03,8.4171387781295D-04,
     1     -5.952379913043012D-04,7.93650793500350248D-04,
     2     -2.777777777777681622553D-03,8.333333333333333331554247D-02,
     3      5.7083835261D-03/
C----------------------------------------------------------------------
      Y = X
      IF ((Y .GT. ZERO) .AND. (Y .LE. XBIG)) THEN
            IF (Y .LE. EPS) THEN
                  RES = -LOG(Y)
               ELSE IF (Y .LE. THRHAL) THEN
C----------------------------------------------------------------------
C  EPS .LT. X .LE. 1.5
C----------------------------------------------------------------------
                  IF (Y .LT. PNT68) THEN
                        CORR = -LOG(Y)
                        XM1 = Y
                     ELSE
                        CORR = ZERO
                        XM1 = (Y - HALF) - HALF
                  END IF
                  IF ((Y .LE. HALF) .OR. (Y .GE. PNT68)) THEN
                        XDEN = ONE
                        XNUM = ZERO
                        DO 140 I = 1, 8
                           XNUM = XNUM*XM1 + P1(I)
                           XDEN = XDEN*XM1 + Q1(I)
  140                   CONTINUE
                        RES = CORR + (XM1 * (D1 + XM1*(XNUM/XDEN)))
                     ELSE
                        XM2 = (Y - HALF) - HALF
                        XDEN = ONE
                        XNUM = ZERO
                        DO 220 I = 1, 8
                           XNUM = XNUM*XM2 + P2(I)
                           XDEN = XDEN*XM2 + Q2(I)
  220                   CONTINUE
                        RES = CORR + XM2 * (D2 + XM2*(XNUM/XDEN))
                  END IF
               ELSE IF (Y .LE. FOUR) THEN
C----------------------------------------------------------------------
C  1.5 .LT. X .LE. 4.0
C----------------------------------------------------------------------
                  XM2 = Y - TWO
                  XDEN = ONE
                  XNUM = ZERO
                  DO 240 I = 1, 8
                     XNUM = XNUM*XM2 + P2(I)
                     XDEN = XDEN*XM2 + Q2(I)
  240             CONTINUE
                  RES = XM2 * (D2 + XM2*(XNUM/XDEN))
               ELSE IF (Y .LE. TWELVE) THEN
C----------------------------------------------------------------------
C  4.0 .LT. X .LE. 12.0
C----------------------------------------------------------------------
                  XM4 = Y - FOUR
                  XDEN = -ONE
                  XNUM = ZERO
                  DO 340 I = 1, 8
                     XNUM = XNUM*XM4 + P4(I)
                     XDEN = XDEN*XM4 + Q4(I)
  340             CONTINUE
                  RES = D4 + XM4*(XNUM/XDEN)
               ELSE 
C----------------------------------------------------------------------
C  Evaluate for argument .GE. 12.0,
C----------------------------------------------------------------------
                  RES = ZERO
                  IF (Y .LE. FRTBIG) THEN
                        RES = C(7)
                        YSQ = Y * Y
                        DO 450 I = 1, 6
                           RES = RES / YSQ + C(I)
  450                   CONTINUE
                  END IF
                  RES = RES/Y
                  CORR = LOG(Y)
                  RES = RES + SQRTPI - HALF*CORR
                  RES = RES + Y*(CORR-ONE)
            END IF
         ELSE
C----------------------------------------------------------------------
C  Return for bad arguments
C----------------------------------------------------------------------
            RES = XINF
      END IF
C----------------------------------------------------------------------
C  Final adjustments and return
C----------------------------------------------------------------------
      DLGAMA = RES
      RETURN
C ---------- Last line of DLGAMA ----------
      END
