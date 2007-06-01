       double precision function bvalue ( t, bcoef, n, k, x, jderiv )
c
c  from  * a practical guide to splines *  by c. de boor
c  calls  interv
c
c  calculates value at x of jderiv-th derivative of spline from b-repr.
c  the spline is taken to be continuous from the right, except at the
c  rightmost knot, where it is taken to be continuous from the left.
c
c  from:   in%"netlibd@research.att.com"  9-aug-1992 13:11:48.46
c  subj:   re: subject: send bvalue from pppack
c  echo "anything free comes with no guarantee!"
c
c******  i n p u t ******
c  t, bcoef, n, k......forms the b-representation of the spline  f  to
c        be evaluated. specifically,
c  t.....knot sequence, of length  n+k, assumed nondecreasing.
c  bcoef.....b-coefficient sequence, of length  n .
c  n.....length of  bcoef  and dimension of spline(k,t),
c        a s s u m e d  positive .
c  k.....order of the spline .
c
c  w a r n i n g . . .   the restriction  k .le. kmax (=20)  is imposed
c        arbitrarily by the dimension statement for  aj, dl, dr  below,
c        but is  n o w h e r e  c h e c k e d  for.
c
c  x.....the point at which to evaluate .
c  jderiv.....integer giving the order of the derivative to be evaluated
c        a s s u m e d  to be zero or positive.
c
c******  o u t p u t  ******
c  bvalue.....the value of the (jderiv)-th derivative of  f  at  x .
c
c******  m e t h o d  ******
c     the nontrivial knot interval  (t(i),t(i+1))  containing  x  is lo-
c  cated with the aid of  interv . the  k  b-coeffs of  f  relevant for
c  this interval are then obtained from  bcoef (or taken to be zero if
c  not explicitly available) and are then differenced  jderiv  times to
c  obtain the b-coeffs of  (d**jderiv)f  relevant for that interval.
c  precisely, with  j = jderiv, we have from x.(12) of the text that
c
c     (d**j)f  =  sum ( bcoef(.,j)*b(.,k-j,t) )
c
c  where
c                   / bcoef(.),                     ,  j .eq. 0
c                   /
c    bcoef(.,j)  =  / bcoef(.,j-1) - bcoef(.-1,j-1)
c                   / ----------------------------- ,  j .gt. 0
c                   /    (t(.+k-j) - t(.))/(k-j)
c
c     then, we use repeatedly the fact that
c
c    sum ( a(.)*b(.,m,t)(x) )  =  sum ( a(.,x)*b(.,m-1,t)(x) )
c  with
c                 (x - t(.))*a(.) + (t(.+m-1) - x)*a(.-1)
c    a(.,x)  =    ---------------------------------------
c                 (x - t(.))      + (t(.+m-1) - x)
c
c  to write  (d**j)f(x)  eventually as a linear combination of b-splines
c  of order  1 , and the coefficient for  b(i,1,t)(x)  must then be the
c  desired number  (d**j)f(x). (see x.(17)-(19) of text).
c
       implicit none
       integer kmax
       parameter (kmax = 50)
       integer jderiv,k,n, i,ilo,imk,j,jc,jcmin,jcmax,jj,kmj,km1,mflag
       integer nmi,jdrvp1
       double precision bcoef(n),t(*),x
       double precision aj(kmax),dl(kmax),dr(kmax),fkmj
c       dimension t(n+k)
c  former fortran standard made it impossible to specify the length
c  of  t precisely without the introduction of otherwise superfluous
c  additional arguments.
       bvalue = 0.d0
       if (jderiv .ge. k)  return
c
c  *** find  i   s.t.   1 .le. i .lt. n+k   and   t(i) .lt. t(i+1)   and
c      t(i) .le. x .lt. t(i+1) . if no such i can be found,  x  lies
c      outside the support of  the spline  f , hence  bvalue = 0.
c      (the asymmetry in this choice of  i  makes  f  rightcontinuous,
c      except  at  t(n+k) where it is leftcontinuous.)
       call interv ( t, n+k, x, i, mflag )
       if (mflag .ne. 0)  return
c  *** if k = 1 (and jderiv = 0), bvalue = bcoef(i).
       km1 = k - 1
       if (km1 .le. 0)  then
          bvalue = bcoef(i)
          return
       end if
c
c  *** store the k b-spline coefficients relevant for the knot interval
c     (t(i),t(i+1)) in aj(1),...,aj(k) and compute dl(j) = x - t(i+1-j),
c     dr(j) = t(i+j) - x, j=1,...,k-1 . set any of the aj not obtainable
c     from input to zero. set any t.s not obtainable equal to t(1) or
c     to t(n+k) appropriately.
    1 jcmin = 1
      imk = i - k
      if (imk .lt. 0)  then
         jcmin = 1 - imk
         do 5 j=1,i
            dl(j) = x - t(i+1-j)
 5       continue 
         do 6 j=i,km1
            aj(k-j) = 0.d0
            dl(j) = dl(i)
 6       continue 
       else
          do 9 j=1,km1
             dl(j) = x - t(i+1-j)
 9        continue 
       end if
c
       jcmax = k
       nmi = n - i
       if (nmi .ge. 0) then
          do 19 j=1,km1
             dr(j) = t(i+j) - x
 19       continue 
       else 
          jcmax = k + nmi
          do 15 j=1,jcmax
             dr(j) = t(i+j) - x
 15       continue 
          do 16 j=jcmax,km1
             aj(j+1) = 0.d0
             dr(j) = dr(jcmax)
 16       continue 
       end if
c
       do 21 jc=jcmin,jcmax
          aj(jc) = bcoef(imk + jc)
 21    continue 
c
c               *** difference the coefficients  jderiv  times.
       if (jderiv .ne. 0) then
          do 24 j=1,jderiv
             kmj  = k-j
             fkmj = kmj
             ilo  = kmj
             do 23 jj=1,kmj
                aj(jj) = ((aj(jj+1) - aj(jj))/(dl(ilo) + dr(jj)))*fkmj
                ilo = ilo - 1
 23          continue 
 24       continue 
       end if
c
c  *** compute value at  x  in (t(i),t(i+1)) of jderiv-th derivative,
c     given its relevant b-spline coeffs in aj(1),...,aj(k-jderiv).
       if (jderiv .ne. km1)  then
          jdrvp1 = jderiv + 1
          do 34 j=jdrvp1,km1
             kmj = k-j
             ilo = kmj
             do 33 jj=1,kmj
                aj(jj) = (aj(jj+1)*dl(ilo) +
     $               aj(jj)*dr(jj))/(dl(ilo)+dr(jj))
                ilo = ilo - 1
 33          continue 
 34       continue 
       end if
       bvalue = aj(1)
c
       return
c  end funtion bvalue
       end
       subroutine interv ( xt, lxt, x, left, mflag )
c  from  * a practical guide to splines *  by c. de boor
c  computes  left = max( i :  xt(i) .lt. xt(lxt) .and.  xt(i) .le. x ).
c
c******  i n p u t  ******
c  xt.....a real sequence, of length  lxt , assumed to be nondecreasing
c  lxt.....number of terms in the sequence  xt .
c  x.....the point whose location with respect to the sequence  xt  is
c        to be determined.
c
c******  o u t p u t  ******
c  left, mflag.....both integers, whose value is
c
c   1     -1      if               x .lt.  xt(1)
c   i      0      if   xt(i)  .le. x .lt. xt(i+1)
c   i      0      if   xt(i)  .lt. x .eq. xt(i+1) .eq. xt(lxt)
c   i      1      if   xt(i)  .lt.        xt(i+1) .eq. xt(lxt) .lt. x
c
c        in particular,  mflag = 0  is the 'usual' case.  mflag .ne. 0
c        indicates that  x  lies outside the closed interval
c        xt(1) .le. y .le. xt(lxt) . the asymmetric treatment of the
c        intervals is due to the decision to make all pp functions cont-
c        inuous from the right, but, by returning  mflag = 0  even if
c        x = xt(lxt), there is the option of having the computed pp function
c        continuous from the left at  xt(lxt) .
c
c******  m e t h o d  ******
c  the program is designed to be efficient in the common situation that
c  it is called repeatedly, with  x  taken from an increasing or decrea-
c  sing sequence. this will happen, e.g., when a pp function is to be
c  graphed. the first guess for  left  is therefore taken to be the val-
c  ue returned at the previous call and stored in the  l o c a l  varia-
c  ble  ilo . a first check ascertains that  ilo .lt. lxt (this is nec-
c  essary since the present call may have nothing to do with the previ-
c  ous call). then, if  xt(ilo) .le. x .lt. xt(ilo+1), we set  left =
c  ilo  and are done after just three comparisons.
c     otherwise, we repeatedly double the difference  istep = ihi - ilo
c  while also moving  ilo  and  ihi  in the direction of  x , until
c                      xt(ilo) .le. x .lt. xt(ihi) ,
c  after which we use bisection to get, in addition, ilo+1 = ihi .
c  left = ilo  is then returned.
c
       implicit none
       integer left,lxt,mflag,   ihi,ilo,istep,middle
       double precision x,xt(lxt)
       save ilo
       data ilo /1/
c
       ihi = ilo + 1
       if (ihi .ge. lxt) then
          if (x .ge. xt(lxt))            go to 110
          if (lxt .le. 1)                go to 90
          ilo = lxt - 1
          ihi = lxt
c
       end if
 20    if (x .ge. xt(ihi))               go to 40
       if (x .ge. xt(ilo))               go to 100
c
c              **** now x .lt. xt(ilo) . decrease  ilo  to capture  x .
      istep = 1
   31    ihi = ilo
         ilo = ihi - istep
         if (ilo .le. 1)                go to 35
         if (x .ge. xt(ilo))            go to 50
         istep = istep*2
                                        go to 31
   35 ilo = 1
      if (x .lt. xt(1))                 go to 90
                                        go to 50
c              **** now x .ge. xt(ihi) . increase  ihi  to capture  x .
   40 istep = 1
   41    ilo = ihi
         ihi = ilo + istep
         if (ihi .ge. lxt)              go to 45
         if (x .lt. xt(ihi))            go to 50
         istep = istep*2
                                        go to 41
   45 if (x .ge. xt(lxt))               go to 110
      ihi = lxt
c
c           **** now xt(ilo) .le. x .lt. xt(ihi) . narrow the interval.
   50 middle = (ilo + ihi)/2
      if (middle .eq. ilo)              go to 100
c     note. it is assumed that middle = ilo in case ihi = ilo+1 .
      if (x .lt. xt(middle))            go to 53
         ilo = middle
                                        go to 50
   53    ihi = middle
                                        go to 50
c**** set output and return.
   90 mflag = -1
       left = 1
       return
 100   mflag = 0
       left = ilo
       return
 110   mflag = 1
       if (x .eq. xt(lxt)) mflag = 0
       left = lxt
 111   if (left .eq. 1)                  return
       left = left - 1
       if (xt(left) .lt. xt(lxt))       return
c  end subroutine interv
      end

