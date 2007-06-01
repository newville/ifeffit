       subroutine wrtprm(iout, ifmt, mlen, name, i1, x1, x2)
c simple dump of named parameters to (unit=iout) with choice of formats
c     ifmt=0  ->   " name  =   x1  % +/- % x2 "
c     ifmt=1  ->   " name  i1  x1  % +/- % x2 "
       character name*(*)
       double precision   x1, x2, ten, order, tiny
       parameter (ten = 10.d0,  tiny = 1.d-8)
       integer   ilen, istrln, iout, ifmt, mlen, i1
       external  istrln
       ilen    = max(mlen,  istrln(name))
       order   = dabs(log(tiny + abs(x1) ) )
       if ((order.gt.ten).and.(ifmt.eq.0)) then
          write(iout,250) name(1:ilen),    x1, x2
       elseif ((order.gt.ten).and.(ifmt.eq.1)) then
          write(iout,200) name(1:ilen),i1, x1, x2
       elseif ((order.le.ten).and.(ifmt.eq.0)) then
          write(iout,150) name(1:ilen),    x1, x2
       elseif ((order.le.ten).and.(ifmt.eq.1)) then
          write(iout,100) name(1:ilen),i1, x1, x2
       end if
 100   format(3x,a,' ',i4,' ',f14.7,'  % +/- %', f14.7)
 150   format(3x,a,' = ',     f14.7,'  % +/- %', f14.7)
 200   format(3x,a,' ',i4,' ',e14.5,'  % +/- %', e14.5)
 250   format(3x,a,' = ',     e14.5,'  % +/- %', e14.5)
       return
       end
