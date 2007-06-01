c
c The Mersenne Twister Algorithm, taken and adapted from MT19937.f:
c Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.
c
c the distribution functions by Matt Newville, looking at sources
c from Python 2.3.3 random.py and Numerical Recipes
c
c A C-program for MT19937: Real number version
c   genrand() generates one pseudorandom real number (double)
c which is uniformly distributed on [0,1]-interval, for each
c call. sgenrand(seed) set initial values to the working area
c of 624 words. Before genrand(), sgenrand(seed) must be
c called once. (seed is any 32-bit integer except for 0).
c Integer generator is obtained by modifying two lines.
c   Coded by Takuji Nishimura, considering the suggestions by
c Topher Cooper and Marc Rieffel in July-Aug. 1997.
c
c This library is free software; you can redistribute it and/or
c modify it under the terms of the GNU Library General Public
c License as published by the Free Software Foundation; either
c version 2 of the License, or (at your option) any later
c version.
c This library is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
c See the GNU Library General Public License for more details.
c You should have received a copy of the GNU Library General
c Public License along with this library; if not, write to the
c Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
c 02111-1307  USA
c
c Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.
c When you use this, send an email to: matumoto@math.keio.ac.jp
c with an appropriate reference to your work.
c
c***********************************************************************
c Fortran translation by Hiroshi Takano.  Jan. 13, 1999.
c
c   genrand()      -> double precision function grnd()
c   sgenrand(seed) -> subroutine sgrnd(seed)
c                     integer seed
c
c This program uses the following non-standard intrinsics.
c   ishft(i,n): If n>0, shifts bits in i by n positions to left.
c               If n<0, shifts bits in i by n positions to right.
c   iand (i,j): Performs logical AND on corresponding bits of i and j.
c   ior  (i,j): Performs inclusive OR on corresponding bits of i and j.
c   ieor (i,j): Performs exclusive OR on corresponding bits of i and j.
c
c***********************************************************************
cc this main() outputs first 1000 generated numbers
c       program main
cc      
c       implicit none
c       integer no, j, k
cc      
c       parameter(no=10000)
c       double precision r(0:7), randmt
c       external randmt
cc
cc      any nonzero integer can be used as a seed
c       call seed_randmt(32054)
c       do 1000 j=0,no-1
c          r(mod(j,8))=randmt()
c          if(mod(j,8).eq.7) then
c             write(*,'(8(f8.6,'' ''))') (r(k),k=0,7)
c        else if(j.eq.no-1) then
c           write(*,'(8(f8.6,'' ''))') (r(k),k=0,mod(no-1,8))
c        endif
c 1000 continue
cc
c      stop
c      end
c***********************************************************************
       subroutine seed_randmt(seed)
c
       implicit none
       integer n, seed
       parameter(n =  624)
       integer mt(0:n-1), mti
c      the array for the state vector
       common /randmt_block/mti,mt
       save   /randmt_block/
c
c      setting initial seeds to mt[N] using
c      the generator Line 25 of Table 1 in
c      [KNUTH 1981, The Art of Computer Programming
c         Vol. 2 (2nd Ed.), pp102]

       if (seed .eq. 0) seed = 4357
       mt(0)= iand(seed,-1)
       do 10 mti=1,n-1
          mt(mti) = iand(69069 * mt(mti-1),-1)
 10    continue
cc       print*, ' seed random numbers to ', seed
c
       return
       end
c***********************************************************************
       double precision function randmt()
c      
       implicit none
       integer n,n1,m,mata,kk
       integer umask,lmask,tmaskb,tmaskc
c
c period parameters
       parameter(n     =  624)
       parameter(n1    =  n+1)
       parameter(m     =  397)
       parameter(mata  = -1727483681) ! constant vector a
       parameter(umask = -2147483648) ! most significant w-r bits
       parameter(lmask =  2147483647) ! least significant r bits
       parameter(tmaskb= -1658038656) ! tempering parameters
       parameter(tmaskc= -272236544)
       double precision scale, scalem1
       parameter(scale = 2.0d0**32, scalem1=2.0d0**32-1.0d0)
c      
       integer mt(0:n-1),mti
c      the array for the state vector
       common /randmt_block/ mti,mt
       save   /randmt_block/
       data   mti/n1/
c                     mti==n+1 means mt[n] is not initialized
c
       integer mag01(0:1),y
       data mag01/0, mata/
       data y/0/
       save mag01,y
c                        mag01(x) = x * mata for x=0,1
c      
       if(mti.ge.n) then
          if (mti.eq.n1) call seed_randmt(4357)
c  generate n words at one time
          do 100 kk=0,n-m-1
             y=ior(iand(mt(kk),umask),iand(mt(kk+1),lmask))
             mt(kk)=ieor(ieor(mt(kk+m),ishft(y,-1)),mag01(iand(y,1)))
 100      continue
          do 200 kk=n-m,n-2
             y=ior(iand(mt(kk),umask),iand(mt(kk+1),lmask))
             mt(kk)=ieor(ieor(mt(kk+(m-n)),ishft(y,-1)),
     $            mag01(iand(y,1)))
 200      continue
          y=ior(iand(mt(n-1),umask),iand(mt(0),lmask))
          mt(n-1)=ieor(ieor(mt(m-1),ishft(y,-1)),mag01(iand(y,1)))
          mti = 0
       endif
c      
       y=mt(mti)
       mti=mti+1
       y=ieor(y,ishft(y,-11))
       y=ieor(y,iand(ishft(y,7),tmaskb))
       y=ieor(y,iand(ishft(y,15),tmaskc))
       y=ieor(y,ishft(y,-18))
c      
       if(y.lt.0) then
          randmt=(dble(y)+scale)/(scalem1)
       else
          randmt=dble(y)/(scalem1)
       endif
c      
       return
       end
ccc
ccc
ccc generate gaussian distribution of random numbers
       double precision function gauss_rand()
c
       implicit none
       double precision gauss_save, rnorm, r1, r2
       double precision randmt, f 
       external         randmt
       logical  toggle
       save     toggle, gauss_save
       data     toggle /.false./
c      
       if (toggle) then
          gauss_rand = gauss_save 
       else
 10       continue 
          r1    = 2 * randmt() - 1
          r2    = 2 * randmt() - 1
          rnorm = r1*r1 + r2*r2
          if (rnorm.ge.1.or.rnorm.eq.0) go to 10
          f = sqrt(-2 * log(rnorm)/rnorm) 
          gauss_save = r1 * f
          gauss_rand = r2 * f
       endif
       toggle = .not.toggle
       return
c end real function gauss_rand
       end

ccc
ccc generate normal distribution of random numbers
       double precision function normal_rand()
c
       implicit none
       double precision rnorm_save,  r1, r2
       double precision randmt, z, half, magic_nv
       parameter (half = 0.5d0)
c                 magic_nv = 4*exp(-0.5)/sqrt(2)
       parameter (magic_nv = 1.715527769921414d0)
       external         randmt
c      
 10    continue 
       r1  = randmt()
       r2  = 1 - randmt()
       z   = magic_nv*(r1-half)/r2
       if (z*z.gt.-4*log(r2)) goto 10
       
       normal_rand = z
       return
c end real function normal_rand
       end
