       subroutine feff_table_array(zstrx, sarrx, mpts, x, y)

       implicit none
       include 'consts.h'
cc       include 'keywrd.h'
       integer          mff_arrs, mpts
       double precision x(maxpts), y(maxpts), small
       parameter       (small=1.d-6)
       parameter       (mff_arrs = 5)
       double precision fdat(mffpts,mff_arrs)
       double precision yk(mffpts), xk(mffpts), qp
       integer          iz, istrln, nwords, is, i, ipos, nfpts
       character*(*)    zstrx, sarrx
       character*(16)   words(3), sym*2, edge*2, sarr*16
       external         istrln
       

       sarr = sarrx
       call lower(sarr)
       call lower(zstrx)
       nwords = 2
       call bwords(zstrx, nwords, words)
       sym  = words(1)
       edge = words(2)
cc       print*, ' feff table: ',  sym, ' ', edge, ' ', sarr

c fdat: =  amp,pha,caps,rep,xlam
       call read_fefftab(sym,edge,mffpts,mff_arrs,xk,fdat,nfpts)
       is = 0
       if (sarr .eq. 'rep')    is = 1
       if (sarr .eq. 'lambda') is = 2
       if (sarr .eq. 'amp')    is = 3
       if (sarr .eq. 'phase')  is = 4
       if (sarr .eq. 'caps')   is = 5

c
c now interpolate feff's k/f(k) data onto input x-array:
       if (is .ge. 1) then
          ipos = 0
          do 500 i = 1, maxpts
             call lintrp(xk, fdat(1,is),nfpts,x(i),ipos,y(i))
 500      continue 
       endif
c  k-dependent path parameters
       return
       end

       subroutine read_fefftab(sym,edge,mffpts,marrs,xk,fdat,nout)
c
c read feff data from the Feff Tables for a given
c atomic symbol and edge 
c 
c output array xk, and fdat:  dimensioned (mffpts,5) 
c   1   rep
c   2   lambda
c   3   amp
c   4   phase
c   5   caps
c
       implicit none
       include 'maxpts.h'
       character*(*)   sym, edge
       character*256   file, path, messg, str, s1, s2,pre*2
       character*(32)  words(2), vers*10, pref*8
       integer         iz, iz_atom, istrln, mffpts, npack
       integer         ier, iex, ierr, nwords, in1, in2
       integer         ilen, lun, i, iread_ky,  j, ntmp
       integer         marrs, nout
       double precision  fdat(mffpts,marrs), xk(mffpts)
       double precision  tmparr(maxpts), xvers
       external iz_atom, istrln

       nout = 0

       iz   = iz_atom(sym)
       in1  = istrln(sym)
       in2  = istrln(edge)
       pref = sym(1:in1)//'_'//edge(1:in2)

       call gettxt('&install_dir', path)
       ilen  = istrln(path)
       write(file,'(a,a,i2.2,a)') path(:ilen),'/fefftab/', iz,'.dat'
       ilen  = istrln(file)
       lun   = -1
       call openfl(lun, file, 'old', iex, ier)
       if ((ier.lt.0).or.(iex.lt.0).or.(lun.le.0)) then
          messg = ' *** feff_tables: cannot open file '//file(1:ilen)
          call warn(3, messg)
          if (lun.gt.0) close(lun)
          return
       end if
c
       do 10 i = 1, mffpts
          xk(i)     = 0
          fdat(i,1) = 0
          fdat(i,2) = 0
          fdat(i,3) = 0
          fdat(i,4) = 0
          fdat(i,5) = 0
 10    continue 
       ilen = iread_ky(lun, pre, str)
       vers  = 'null'
       npack = 0
       if ((pre.eq.'#:').and.(str(1:20).eq.'IFEFFIT SAVE File: v')) then
          vers = str(21:25)
          call triml(vers)
          call lower(vers)
          call str2dp(vers,xvers,ier)
          s2   = str(26:)
          call lower(s2)
          nwords = 2
          call bwords(s2,nwords,words)
          if (words(1).eq.'npad') call str2in(words(2),npack,ierr)
       endif
       if ((npack.le.0).or.(npack.ge.16).or.(vers.eq.'null')) goto 1010
c next line must be 'build'
       if (xvers.ge.1.01) then
          ilen = iread_ky(lun, pre, str)
          if ((pre.ne.'#:').or.(str(1:5).ne.'build')) goto 1010
       endif
c skip over everything until arrays
 120   continue 
       ilen = iread_ky(lun, pre, str)
       if (ilen.lt.0)         goto 800
       if ((pre.ne.'#:').or.(str(1:6).ne.'arrays')) goto 120

 200   continue 
       ilen = iread_ky(lun, pre,str)
       if (ilen.lt.0) goto 800
       in1 = index(str,'[')
       in2 = index(str,']')
       call str2in(str(in1+1:in2-1),ntmp, ierr)
       str = str(in2+1:)
       call triml(str)
       if (pre.eq.'#%') then
c read array data
          j  = index(str,'.')
          s1 = str(1:j-1)
          s2 = str(j+1:)
          call triml(s1)
          call triml(s2)
          call rdpadd(lun,npack,tmparr, ntmp)
          if (s2 .eq. 'k') then
             nout = ntmp
             do 310 i = 1, ntmp
                xk(i) = tmparr(i)
 310         continue 
          elseif (s2 .eq. 'rep') then
             do 320 i = 1, ntmp
                fdat(i,1) = tmparr(i)
 320         continue 
          elseif (s2 .eq. 'lambda') then
             do 330 i = 1, ntmp
                fdat(i,2) = tmparr(i)
 330         continue 
          elseif ((s1 .eq. pref) .and. (s2 .eq. 'amp')) then
             do 340 i = 1, ntmp
                fdat(i,3) = tmparr(i)
 340         continue 
          elseif ((s1 .eq. pref) .and. (s2 .eq. 'phase')) then
             do 350 i = 1, ntmp
                fdat(i,4) = tmparr(i)
 350         continue 
          elseif ((s1 .eq. pref) .and. (s2 .eq. 'caps')) then
             do 360 i = 1, ntmp
                fdat(i,5) = tmparr(i)
 360         continue 
          endif

       else
          goto 1010
       endif
       go to 200

 800   continue 
c


       close(lun)
       return
 1010  continue 
       call warn(3,' *** bad data in feff tables')
       end


       integer function iz_atom(sym)
c
c  copyright 1993 university of washington     matt newville and bruce ravel
c---------------------------------------------------------------------------

c     returns z number given atomic symbol: default is 0
       character*2 symbol(103), sym, sy
c
       data (symbol(i),i=1,103) /'h','he','li','be','b','c','n','o',
     $      'f','ne','na','mg','al','si','p','s','cl','ar','k','ca',
     $      'sc','ti','v','cr','mn','fe','co','ni','cu','zn','ga',
     $      'ge','as','se','br','kr','rb','sr','y','zr','nb','mo',
     $      'tc','ru','rh','pd','ag','cd','in','sn','sb','te','i',
     $      'xe','cs','ba','la','ce','pr','nd','pm','sm','eu','gd',
     $      'tb','dy','ho','er','tm','yb','lu','hf','ta','w','re',
     $      'os','ir','pt','au','hg','tl','pb','bi','po','at','rn',
     $      'fr','ra','ac','th','pa','u','np','pu','am','cm','bk',
     $      'cf','es','fm','md','no','lr'/

       sy   = sym
       call lower(sy)
       iz_atom = 0
       do 110 i=1,103
          if (sy.eq.symbol(i)) then
             iz_atom = i
             goto 120
          end if
 110   continue
 120   return
c end integer function is2z
       end
