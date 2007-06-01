       integer function clcalc(iz, path, npts, energy, fp, fpp)
c      cromer-libermann calculation of anomalous scattering factors 
c arguments:
c   iz      atomic number of element                       [in]
c   path    path to cromer-libermann files                 [in]
c   npts    number of elements in energy array             [in]
c   energy  array of energies at which to calculate f'/f'' [in]
c   fp      real part of anomalous scattering   (f')       [out]
c   fpp     imag part of anomalous scattering   (f'')      [out]
c
c notes: 
c   1  energy array is in eV
c   2  this code is based on, and modified from the cowan-brennan 
c      routines.  data statements were simplified and rearranged, 
c      code was cleaned up to be more in keeping with f77 standard
c
c  matthew newville oct 1996
c  matthew newville oct 2000
       implicit none
       integer  iz, npts, iread
       integer  i, j, k, nxpts, norb, nparm(24)
       character*(*)     path
       double precision  energy(*), fp(*), fpp(*)
       double precision  bena(24),  xnrg(24,11),  xsc(24,11)
       double precision  corr, ener, f1, f2
       integer rcldat
       external rcldat
       clcalc = 0
c  initialize the trivial parts of xnrg:
c  (non-trivial parts are given in the data statements above)
       iread = rcldat(path,iz,norb,corr,nparm,bena,xnrg,xsc)
c
c    rcldat returns 0 on success
       if (iread.eq.0) then
c calculate fp and fpp for each energy point
          do 200 i = 1, npts
             ener = energy(i) / 1000
             call cromer(iz,ener,nparm,norb,bena,xnrg,xsc,f1,f2)
             fp(i)  = f1 - corr
             fpp(i) = f2
 200      continue 
       end if
       return 
       end
cc
       integer function rcldat(path,iz,norb,corr,nparms,bena,xnrg,xsc)
       implicit none
       character*(*) path, pth*256, file*256, line*256
       character*32 words(8)
       integer istrln, nxtmp, npack
       integer  iz, norb, nparms(24), lun, ilen, iread, nwords
       integer  i, ier1, ier2, ier3,ier4, ier5, ier6, j
       double precision corr, bena(24), xnrg(24,11), xsc(24,11)
       double precision relcor, kpcor, xtmp(19)
       double precision xnrd1, xnrd2, xnrd3, xnrd4, xnrd5, zero
       parameter (xnrd1  = 80.d0, xnrd2 = 26.7d0, xnrd3 = 8.9d0)
       parameter (xnrd4  = 3.0d0, xnrd5 = 1.0d0, zero  = 0.d0)
       parameter (npack  = 8)
       external iread, istrln

       norb = 0
       corr = zero
       do 30 i = 1, 24
          bena(i)   = zero
          nparms(i) = 0
          do 25 j = 1, 11
             xsc(i,j)  = zero
             xnrg(i,j) = zero
 25       continue 
 30    continue 
       rcldat = 1
       pth = path
       call triml(pth)
       ilen = istrln(path)
       write(file,'(a,i2.2,a)') path(:ilen),iz,'.dat'
       lun = 10
       open(unit=lun,file=file, status='old', err=700)
c
c read ignored title line
       ilen = iread(lun,line)
       call triml(line)
       if ((line(1:1).ne.'#').or.(index(line,'PAD').eq.0)) goto 700
c
c read iz, norb, relcor, kpcor
       ilen = iread(lun,line)
       if (ilen.le.0) goto 700
       nwords = 4
       call bwords(line,nwords,words)
       call str2in(words(1),iz,    ier1)
       call str2in(words(2),norb,  ier2)
       call str2dp(words(3),relcor,ier3)
       call str2dp(words(4),kpcor, ier4)
       corr = relcor - kpcor
       if ( (ier1.ne.0).or.(ier2.ne.0).or.
     $      (ier3.ne.0).or.(ier4.ne.0)) goto 700
c
c read PADified data : note careful reading of integer nparms!
       nxtmp = 19
       do 200 i = 1, norb
          xnrg(i,1) = xnrd1
          xnrg(i,2) = xnrd2
          xnrg(i,3) = xnrd3
          xnrg(i,4) = xnrd4
          xnrg(i,5) = xnrd5
          call rdpadd(lun,npack,xtmp,nxtmp)
          nparms(i) = int(xtmp(1) + 0.1)
          bena(i)   = xtmp(2)
          xnrg(i,6) = xtmp(3)   
          xnrg(i,7) = xtmp(4)   
          xnrg(i,8) = xtmp(5)   
          xnrg(i,9) = xtmp(6)   
          xnrg(i,10)= xtmp(7)   
          xnrg(i,11)= xtmp(8)   
          xsc(i,1)  = xtmp(9)   
          xsc(i,2)  = xtmp(10)  
          xsc(i,3)  = xtmp(11)  
          xsc(i,4)  = xtmp(12)  
          xsc(i,5)  = xtmp(13)  
          xsc(i,6)  = xtmp(14)  
          xsc(i,7)  = xtmp(15)  
          xsc(i,8)  = xtmp(16)  
          xsc(i,9)  = xtmp(17)  
          xsc(i,10) = xtmp(18)  
          xsc(i,11) = xtmp(19)  
 200   continue 
       close(lun)
       rcldat = 0
       return
 700   continue  
       close(lun)
       rcldat = -1       
       return
       end
       subroutine cromer(iz,ener,nparms,norb,benaz,xnrg,xsc,f1,f2)
c modified from cowan-brennan routines      matt newville oct 1996
c this routine reads data for f' and f" according to an
c algorithm by cromer and lieberman, given to fuoss.
c converted to direct access file by brennan
c converted to internal data 3-may-1993 smb
       implicit none
       integer        iz, irb, ipr, icount,i0,inxs, norb, nparms(24)
       double precision  benaz(24), xnrg(24,11), xsc(24,11)
       double precision  ener, f1, f2, zero, fourpi
       double precision  f1orb, f2orb, en_s(11), xs_s(11), aknint
       double precision  xlnnrg(11),xln_xs(11),en_int(5), xs_int(5)
       double precision  xsedga, f1corr, xlne, energa, bena, xsb 
       double precision  var, au, kev2ry, fscinv, tiny, tinlog 
       double precision  finepi, sigma0, sigma1, sigma2, sigma3, gauss
       parameter (zero=0, fourpi=12.56637061435917d0, tiny =  1.d-13)
       parameter (au = 2.80022d+7 ,kev2ry = 0.0272113834d0) 
c finepi = 1/(4*alpha*pi**2)
       parameter (finepi = 3.47116243d0, fscinv =137.036d0 ) 
       common /gaus/ xsb,bena,xs_int, energa, xsedga,icount
       external sigma0,sigma1,sigma2,sigma3
       save
c      executable code
c ener is in kev
       xlne   = log(ener)
       energa = ener /  kev2ry
       f1     = zero
       f2     = zero
       tinlog = log(tiny)
      
c      main loop through the orbitals
       do 400 irb=1,norb
          icount= 6
          f1orb = zero
          f1corr= zero
          f2orb = zero
          xsb   = zero
          bena  = benaz(irb)
          if (nparms(irb) .eq. 11) xsedga = xsc(irb,11)/ au
         
c      also copy subset into second array
          do 110 ipr=6,10
             xs_int(ipr-5) = xsc(irb,ipr)/ au
             en_int(ipr-5) = xnrg(irb,ipr)
 110       continue 
          
c   the sorting routine messes up subsequent calls with same energy
c   so copy to second array before sorting.
          do 150 ipr=1,nparms(irb)
             xs_s(ipr) = xsc(irb,ipr)
             en_s(ipr) = xnrg(irb,ipr)
 150       continue 
          call sort(nparms(irb),en_s,xs_s)
          call sort(5,en_int,xs_int)
c      convert to log of energy,xsect
          do 190 ipr=1,nparms(irb)
             xlnnrg(ipr) = log(en_s(ipr))
             xln_xs(ipr) = log(max(tiny,xs_s(ipr)))
             if (xln_xs(ipr).le.tinlog) xln_xs(ipr) = zero
 190      continue 
c
          if (bena .le. energa) then
             do 250 i0 = 1, nparms(irb)
                if (abs(xln_xs(i0)) .ge. tiny ) go to 255
 250         continue
 255         continue
             inxs = nparms(irb) - i0 + 1
             xsb  = exp(aknint(xlne,inxs,xlnnrg(i0),xln_xs(i0)))/au
             f2orb= fscinv * energa * xsb / fourpi
             var  = energa-bena
             if (abs(var). le. tiny) var = 1
             f1corr = - finepi * xsb * energa * log((energa+bena)/var)
          end if
c
          if((bena.gt.energa).and.(nparms(irb).eq.11)) then
             f1orb  = gauss(sigma3) 
             f1corr = finepi * xsedga * bena**2 * log((-bena+energa)
     $            /(-bena-energa)) / energa 
          else
             if (nparms(irb).eq.11)   then
                f1orb = gauss(sigma0)
             elseif ((nparms(irb).eq.10).and.
     $               (iz.ge.79).and.(irb.eq.1)) then
                f1orb = gauss(sigma1)
             else 
                f1orb = gauss(sigma2)
             end if
          end if
          f1 = f1 + f1orb * 2 * finepi + f1corr
          f2 = f2 + f2orb
 400   continue 
c      this is the end of the loop over orbits
     
c      
c      note: the jensen correction to f' was subsequently shown to be incorrect
c      (see l. kissel and r.h. pratt, acta cryst. a46, 170 (1990))
c      and that the relativistic correction that ludwig used is also
c      wrong.  this section retained as comments for historical reasons.
c      
c      jensen_cor = -0.5*float(iz)
c      1			*(energa/fscinv**2)**2
c      
c      subtract relcor ala ludwig and change back to real*4
c      
c      f1 = sumf1+jensen_cor-relcor(iz)
c      
c      kissel and pratt give better corrections.  the relativistic correction
c      that ludwig used is (5/3)(e_tot/mc^2).  kissel and pratt say that this
c      should be simply (e_tot/mc^2), but their correction (kpcor) apparently
c      takes this into account.  so we can use the old relcor and simply add
c      the (energy independent) kpcor term:
c      
       return
       end
       double precision function sigma0( x)
       implicit none
       double precision  x, xsb, bena, xs_int(5)
       double precision  energa, d_prod, xsedga, tiny
       parameter(tiny=1.d-30)
       integer icount
       common /gaus/ xsb, bena, xs_int, energa, xsedga, icount
       save
c      executable code
       icount = icount-1
       sigma0 = xs_int(icount)* bena/(x*x)
       d_prod = (energa*x)**2 - bena**2
       if(abs( d_prod) .gt. tiny)
     $      sigma0 =bena * ( sigma0 * bena - xsb* energa**2)/ d_prod
       return
       end
c***********************************************************************
       double precision function sigma1( x)
       implicit none
       double precision  x, xsb, bena, xs_int(5)
       double precision  energa, xsedga, half
       parameter (half = 0.5d0)
       integer icount
       common /gaus/ xsb, bena, xs_int,
     $      energa, xsedga, icount
       save
c      executable code
       icount = icount-1
       sigma1 = half* bena**3* xs_int( icount)
     $ /( sqrt(x)*( energa**2* x**2- bena**2* x))
       return
       end
c***********************************************************************
       double precision function sigma2(x)
       implicit none
       double precision x, zero, tiny, p1, denom, eps
       double precision xsb, bena, xs_int(5), energa, xsedga
       integer icount
       common /gaus/ xsb, bena, xs_int, energa, xsedga,icount
       parameter (zero = 0, tiny = 1.d-18, eps = 1.d-5, p1 = 1.001d0)
       save
       icount=icount-1
c     code modified by chris t. chantler, may 12-1992
c     code modified by matt newville  oct 1996 
       if ((abs(x).lt.tiny).or.(energa.lt.tiny)) then
          sigma2= zero
       elseif (abs(xs_int(icount)-xsb).lt.tiny) then
          sigma2=-2*xs_int(icount)*bena/x**3
       else
          denom= x**3*energa**2-bena**2/ x
          if (abs(denom).lt.eps) then
cc chantler:        sigma2=-2*xs_int(icount)*bena/x**3
             denom= x**3*(energa*p1)**2-bena**2/ x
cc             print*, ' weird point at e =  ', energa * 27.21d0
          end if
          sigma2= 2*(xs_int(icount)*(bena/x)**3/x-
     $         bena* xsb* energa**2)/ denom
       endif
       return
       end
c***********************************************************************
       double precision function sigma3( x)
       implicit none
       double precision  x, xsb, bena, xs_int(5), energa, xsedga
       integer icount
       common /gaus/ xsb,bena,xs_int, energa, xsedga,icount
       save
c      executable code
       icount = icount-1
       sigma3 = bena**3*( xs_int( icount)
     $      - xsedga* x**2)/( x**2*( x**2* energa**2- bena**2))
       return
       end
c***********************************************************************
       subroutine lgndr (index,dbb,dcc)
       implicit none
       integer index, ip
       double precision  dbb, dcc, const, d_x(2), d_a(3)
       double precision half,zero,one
       parameter(half = 0.5d0, zero = 0d0, one = 1d0)
       data d_x(1), d_x(2) /.04691007703067d0, .23076534494716d0/
       data d_a(1), d_a(2) /.11846344252810d0, .23931433524968d0/
       data d_a(3)         /.28444444444444d0/

c      executable code
c      warning! this routine has been stripped so it is only useful
c      with abs$cromer in this set of routines.
       dcc = half
       const=zero
       ip= index
c      ip limited to 1,2,3
       if ( ip .gt. 3) then
          ip   = 6 - ip
          const= -one
       end if
       dbb = d_a(ip)
       if( ip .eq. 3) return
       dcc= -const+ sign( d_x(ip), const)
       return
       end
c***********************************************************************
       double precision function gauss (sigma)
       implicit none
       integer i
       double precision  b, c, sigma, zero
       parameter (zero  = 0.d0)
       external sigma
       gauss = zero
       do 10 i=1,5
          call lgndr( i, b, c)
          gauss = gauss + b * sigma(c)
 10    continue 
       return
       end
c*************************************************************
c***********************************************
c      bubble sort.  largest becomes last
       subroutine sort (n,a,b)
       implicit none
       integer i, n, j
       double precision  a(*), b(*), x, y
       
       do 11 i=1,n-1
          do 10 j=i+1,n
             if(a(j).lt.a(i)) then
        	x=a(j)
        	y=a(i)
        	a(i)=x
        	a(j)=y
        	x=b(j)
        	y=b(i)
        	b(i)=x
        	b(j)=y
             end if
 10       continue 
 11    continue 
       return
       end
c      aitken repeated interpolation
c      xlne   = abscissa at which interpolation is desired
c      xlnnrg = vector of n values of abscissa
c      xln_xs = vector of n values of ordinate
c      t      = temporary storage vector of 4*(m+1) locations)
       double precision function aknint( xlne, n, xlnnrg, xln_xs)
       implicit none
       integer n, i, ii, j
       double precision  t(20), xlne, xlnnrg(n), xln_xs( n)
c      executable code
       if(n .le. 2) then
          write(*,'(a)') ' aknint:  too few points, funct=y(1)'
          aknint = xln_xs(1)
          return
       end if
       if (xlnnrg(2) .gt. xlnnrg(1)) then
          do 10 i = 1, n
             if (xlnnrg(i) .ge. xlne) go to 30
 10       continue 
       else
          do 20 i = 1, n
             if (xlnnrg(i) .le. xlne) go to 30
 20       continue 
       end if
 30    continue 
       ii = min(n-2, max(1, i-1))
       do 40 i= ii, ii+2
          t(i-ii+1) = xln_xs(i)
          t(i-ii+4) = xlnnrg(i)- xlne
 40    continue 
       
       do 70 i=1,2
          do 60 j=i+1,3
             t(j) = ( t(i)*t(j+3)-t(j)*t(i+3))
     $            /( xlnnrg( j + ii - 1)- xlnnrg( i + ii - 1))
 60       continue 
 70    continue 
       aknint= t(3)
       return
       end
