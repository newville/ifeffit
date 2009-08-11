       subroutine iff_erase(str)
c
c//////////////////////////////////////////////////////////////////////
c Copyright (c) 1997--2000 Matthew Newville, The University of Chicago
c Copyright (c) 1992--1996 Matthew Newville, University of Washington
c
c Permission to use and redistribute the source code or binary forms of
c this software and its documentation, with or without modification is
c hereby granted provided that the above notice of copyright, these
c terms of use, and the disclaimer of warranty below appear in the
c source code and documentation, and that none of the names of The
c University of Chicago, The University of Washington, or the authors
c appear in advertising or endorsement of works derived from this
c software without specific prior written permission from all parties.
c
c THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
c EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
c MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
c IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
c CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
c TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
c SOFTWARE OR THE USE OR OTHER DEALINGS IN THIS SOFTWARE.
c//////////////////////////////////////////////////////////////////////
c  erase an array or scalar values for ifeffit
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'encod.h'
       include 'feffit.h'
       save

       character*(*) str, s*512
       integer nwrds, iw, i, j, k, istrln, ilen, id,np, ip
       integer ipths(max_pathindex), ier
       double precision getsca
       external istrln, getsca

       nkeys = mkeys
       do 5 i = 1, nkeys
          keys(i) = blank
 5     continue 
       call bwords(str, nkeys, keys)
cc       do i = 1, nkeys
cc          print*, keys(i)(1:30)
cc       enddo
c check if this is a vector or scalar
       iw  = 0
 10    continue 
       iw = iw + 1
       if (iw.gt.nkeys)  then
          call iff_sync
          return
       endif
       k = istrln(keys(iw))
       s = keys(iw)(1:k)
       call lower(s)
       if (s.eq.'@arrays') then 
          call erase_array(-1,.false.)
       elseif (s.eq.'@scalars') then 
cc          print*, ' erasing scalars'
          do 40 i = 1, maxsca
             call erase_scalar( i )
 40       continue 
       elseif (s.eq.'@strings') then 
          do 50 i = 1, maxtxt
             txtnam(i) = ' '
             text(i)   = ' '
 50       continue 
       elseif (s.eq.'@group') then 
          iw    = iw + 1
          tmpstr= keys(iw)
          ilen  = istrln(tmpstr)
cc
cc note that erase_array will move arrays around, and will
cc drop them down by 1, so a simple loop would miss some
cc this loop counts up to maxarr, backstepping for each
cc erased array.
          i = 0
 65       continue 
             i = i + 1
             if (i.ge.maxarr) go to 70
cc             print*,' i = ', i
cc             call isharr(i)
             j = index(arrnam(i),'.')
             if (j.gt.0) then
                if(tmpstr(1:ilen) .eq. arrnam(i)(1:j-1)) then
                   call erase_array(i,.false.)
                   i = max(0,i-1)
                endif
             endif
             go to 65
 70       continue 
       elseif (s.eq.'@paths') then 
          call str2il('all', max_pathindex,np,ipths,ier)
cc          print*, ' erasing ', np
          do 120 ip = 1, np
             call erase_path(ipths(ip))
 120      continue 
       elseif (s.eq.'@path') then 
          s    = blank
          ilen = 1
          id   = int(getsca('data_set',1))
          if (id.le.0) id = 1
          do 160 i = iw+1, nkeys
             k  = istrln(keys(i))
             s  = s(1:ilen)//keys(i)(1:k)//','
             ilen = ilen + k + 1
 160      continue 
          call str2il(s(1:ilen), max_pathindex,np,ipths,ier)
          do 180 ip = 1, np
             call erase_path(ipths(ip))
 180      continue 
          iw = iw + nkeys
c
c
c single array or scalar
       elseif (index(s,'.').eq.0) then
          if (s(1:1).eq.'$') then
             do 840 i = 1, maxtxt
                if (s(2:).eq.txtnam(i)) call erase_string(i)
 840         continue           
          else 
             do 850 i = 1, maxsca
                if (s.eq.scanam(i)) call erase_scalar(i)
 850         continue 
          end if
       else
          do 890 i = 1, maxarr-1
             if (s.eq.arrnam(i)) call erase_array(i,.false.)
 890      continue 
       end if
       go to 10
c  re-sync    
       call iff_sync
       return
       end

       subroutine erase_array(index,dosync)
c
c erase an array by index
c  giving index<0 will erase all arrays
c
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       integer  i, k, npdiff, np, nd, npx,nn,n, index
       logical dosync
       save
c
       i = index
       if (i .lt. 0) then
          do 5 i = 1, maxarr
             arrnam(i)  = blank
             arrfrm(i)  = blank
             arrmax(i)  = 0
             arrmin(i)  = 0
             icdarr(1,i)= 0
             icdarr(2,i)= 0
             narray(i)  = 0
             nparr(i)   = -1
 5        continue 
          npnext= 1
          return
       endif

cc       print*, ' erase array ', i, ' ' , arrnam(i)(1:20), nparr(i)
c      
c find the next array pointer and save the pointer difference 
c so the other arrays can be dropped down the stack
       npx   = nparr(i)
       n     = npx + narray(i)
       nd    = maxheap_array
       do 10 k = 1, maxarr
          if ((narray(k).gt.0).and.(nparr(k).ge.n)) then
             nd = min(nd, nparr(k))
          endif
 10    continue 
       npdiff = nd - npx
c
c the actual erase of the current array
       arrnam(i)  = blank
       arrfrm(i)  = blank
       arrmax(i)  = 0
       arrmin(i)  = 0
       icdarr(1,i)= 0
       icdarr(2,i)= 0
       narray(i)  = 0
       nparr(i)   = -1
c
c drop all other array pointers by this difference 
c to reclaim space in array heap 
cc       print*, ' drop  nparrs greater than ', npx, ' by ', npdiff
       if (npdiff .gt. 0) then
          do 280 k = 1, maxarr
             np = nparr(k)
             nd = np - npdiff
             if ((np.gt.npx)) then 
                do 220 i = 0, narray(k) - 1
                   array(nd+i) = array(np+i)
 220            continue 
                nparr(k) = nd
             endif
 280      continue 
       endif
c
c re-determine position of npnext: the next array pointer
       npnext= 1
       do 320 i = 1, maxarr
          if (arrnam(i).ne.blank)  then
             np     = (1+((nparr(i) + narray(i))/2))*2
             npnext = max(1,max(npnext, np))
          endif
 320   continue 
cc       print*, 'end of erase_array: npnext: ', npnext, dosync
       if (dosync) then 
          call iff_sync
       endif
       return
       end
       
       subroutine erase_scalar(i)
c erase a scalar by index
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       integer i
       character*96 sname
       save
       
       sname  =  scanam(i)
       if ((sname .ne. 'pi').and.(sname .ne. 'etok').and.
     $      (sname(1:1).ne.'&')) then
          scanam(i) = blank
          scafrm(i) = blank
       endif
       return
       end

       subroutine erase_string(i)
c erase a string
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       integer i
       character*96 sname
       save
       sname  = txtnam(i)
       if (sname(1:1).ne.'&') then
          txtnam(i) = blank
          text(i)   = blank
       endif
       return
       end

       subroutine erase_path(iup)
c erase a path
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'encod.h'
       include 'fefdat.h'
       include 'feffit.h'
       include 'pthpar.h'
       save
       integer  iup, id, inpath, u2ipth, jfeff, i
       integer  ntmp, ilen,  istrln
       logical erase_fefffile
       external istrln, u2ipth

       inpath = u2ipth(iup)
       jfeff  = jpthff(inpath)

       if (inpath .le. 0) return
       pthlab(inpath) = ''
       do 50 i = 1, mpthpr
          icdpar(1,i,inpath) = 0
          icdpar(2,i,inpath) = 0
 50    continue 

c
c delete feff index, saving a temporary copy
       jpthff(inpath) = 0

c sets whether this feff file is used for another path
       erase_fefffile = .true.

       do 70 i = 0, mpaths
          if (iup .eq. jdtusr(i)) jdtpth(i) = 0
          if (jfeff.eq.jpthff(i)) erase_fefffile = .false.
 70    continue 

       if (erase_fefffile) then
          refpth(jfeff) = zero
          degpth(jfeff) = zero
          feffil(jfeff) = blank
          fpthid(jfeff) = blank
          lffred(jfeff) = .false.
          iffrec(jfeff) = 0
          do 90 i = 1, mffpts 
             theamp(i,jfeff) = zero
             thepha(i,jfeff) = zero
             qfeff(i,jfeff) = zero
             realp(i,jfeff) = zero
             thcaps(i,jfeff) = zero
 90       continue 
          do 95 i = 1, nlgpth(jfeff)
             ratpth(1,0,i) = 0
             ratpth(2,0,i) = 0
             ratpth(3,0,i) = 0
 95       continue 
          nlgpth(jfeff) = 0
       end if

       return
       end










