       subroutine fefinp

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

c    read path information from feff files:
c    reads either feffnnnn.dat files (feff5 or higher) or the
c    ascii feff.bin files from feff8
c
c    copyright 1998       matt newville
       implicit none
       include 'consts.h'
       include 'fefdat.h'
cc       implicit double precision (a-h,o-z)

       integer  mlegx, mptsx, mwords, nwords, j, iunit, l0
       integer  itmp, nleg, nepts, iex, ierr, ndoc, npot, npts
       integer  mpts, mfil, mtitle, mleg, npack
       integer  mdocx, ipth, ilen, i, ntitle, istrln

       double precision  rf2b, archi, aichi, arck, aick, rnrmav, xlmin
       logical        exist
       parameter (xlmin = 1.d-8)
       parameter(mlegx = 10, mptsx= 128, mwords = 32, mdocx=16)
       character*64  filnam*256, stat*10, str*128, words(mwords)
       character*128 doc(mdocx), messg, tmpstr*256
       character*2 at_symbol
       integer          izpot(0:mlegx)
       complex*16       phc(mptsx), ck(mptsx)
       double precision xk(mptsx), achi(mptsx), phchi(mptsx)
       double precision beta(mlegx)
       double precision eps, phff, phffo, xlam, reff, bohr
       parameter (eps = 1.d-12, bohr = 0.529 177 249d0)
       parameter (stat = 'old')
       integer  iread
       external iread, at_symbol
       npack = 8
c  feff.dat data extraction
c  loop to get data from at most maxpth paths
       ipth = 0
cc       print*, " FEFINP " 
  50   continue
       ipth = ipth + 1
c   skip unused paths
       if ((ipth.le.mfffil).and.(feffil(ipth).ne.blank)) then
cc          print*, ' fefinp: ipth = ', ipth, ' : ', feffil(ipth)(1:30)
cc          print*,  ipth, iffrec(ipth), lffred(ipth)
c   get next file name from feffil, check that it exists
          exist  = .false.
          filnam = feffil(ipth)
          call triml (filnam)
          ilen = max(1, istrln(filnam))
          inquire(file = filnam,  exist = exist)
c  failed to find this file: stop with warning message
          if (.not.exist) then
             call warn(3,'  warning: could not find FEFF file: '
     $            //filnam)

             feffil(ipth) = blank
c  read data from file
          elseif (iffrec(ipth).eq.0) then
             if (.not.lffred(ipth)) then
c      feff5-style feffnnnn.dat file
c      initialize this path
                nlgpth(ipth) = 2
                refpth(ipth) = zero 
                rwgpth(ipth) = zero
                degpth(ipth) = zero
                do 70 i = 0, maxleg
                   ratpth(1,i,ipth) = zero
                   iptpth(i,ipth)  = 0
                   izpth(i,ipth)  = 0
 70             continue 
                do 80 i = 1, mffpts
                   qfeff(i,ipth)  = zero
                   theamp(i,ipth) = zero
                   thepha(i,ipth) = zero
                   xlamb(i,ipth)  = 1
                   realp(i,ipth)  = zero
 80             continue 
                call echo( '  reading  '//filnam(1:ilen))
cc                print*, ' for ipth = ', ipth
                call rdffdt(filnam,mffttl,maxleg,mffpts,
     $               ntitle,nlgpth(ipth),
     $               npts,fefttl(1,ipth), refpth(ipth), rwgpth(ipth),
     $               degpth(ipth), ratpth(1,0,ipth), iptpth(0,ipth),
     $               izpth(0,ipth), qfeff(1,ipth),theamp(1,ipth),
     $               thepha(1,ipth), thcaps(1,ipth), thsaps(1,ipth), 
     $               xlamb(1,ipth), realp(1,ipth))
                lffred(ipth) = .true.

                nffpts(ipth) = npts
c                print*, 'chipth',  qfeff(1,1),   qfeff(10,1)
c                print*, 'chipth',  mffpts, npts
c                print*, qfeff(npts-1,1),qfeff(npts,1)
c                print*, qfeff(mffpts-1,1),qfeff(mffpts,1)

             end if
          elseif (iffrec(ipth).ne.0) then
c feff8-style feff.bin
             iunit = 0
             call openfl(iunit,filnam,stat,iex,ierr)
             if ((iex.lt.0).or.(ierr.lt.0))
     $            call warn(3,'  error reading file: '//filnam)
c read top of feff.bin for all the records from this file
             call rdfb1(filnam,iunit,mdocx,maxleg,mffpts,npack,
     $            ndoc,npot,
     $            npts, rnrmav, l0, doc,izpot,phc,ck,xk)
c skip to next record within this file
 200         continue
             ilen = iread(iunit, str)
             if (ilen.lt.0) goto 500
             if (str(1:2).ne.'##')   go to 200
             nwords = 1
             call bwords(str(3:),nwords,words)
             call str2in(words(1) , itmp, ierr)
c  skip this record (and go to next) if this isn't the record we want
             if (itmp.ne.iffrec(ipth)) go to 200
c we found the right record:  read in path information
             ilen = istrln(filnam)
             write(messg,'(a,1x,a,1x,i5)')
     $            filnam(1:ilen), ',', iffrec(ipth)
             ilen = istrln(messg)
             call echo( '              '//messg(1:ilen))
             rwgpth(ipth) = rnrmav
             do 305 i = 1, ndoc
                fefttl(i,ipth) =  doc(i)
 305         continue
             nwords = mwords
             call bwords(str(3:),nwords,words)
             call str2in(words(2), nleg, ierr)
             nlgpth(ipth) = nleg
             call str2dp(words(3), degpth(ipth), ierr)
             call str2in(words(4), nepts, ierr)
             call str2dp(words(5), refpth(ipth), ierr)
             reff  = refpth(ipth) / bohr
             do 320 j = 1, nleg
                call str2in(words(6+j),i,ierr)
                iptpth(j,ipth) = i
                izpth(j,ipth) = izpot(i)
 320         continue
             iptpth(0,ipth) = 0
             izpth(0,ipth)  = izpot(0)
             call rdpadd(iunit,npack, ratpth(1,1,ipth),3*nleg)
             do 340 i = 0, nleg
                do 335 j = 1, 3
                   ratpth(j,i,ipth) = ratpth(j,i,ipth) * bohr
 335            continue
 340         continue
c  note that we really don't care about beta, eta, ri arrays, so we'll
c  just skip them here:
             call rdpadd(iunit,npack,beta,nleg)
             call rdpadd(iunit,npack,beta,nleg)
             call rdpadd(iunit,npack,beta,nleg)
c but we really want these arrays (amplitude and phase)
c
c note that this version of feff.bin does not save 
c central-atom and scattering-atom phase-shifts separately.
             call rdpadd(iunit,npack,achi,nepts)
             call rdpadd(iunit,npack,phchi,nepts)
c
             do 390 j = nepts+1, mptsx
                achi(j)  = zero
                phchi(j) = zero
 390         continue
c now convert this into same info as in feff.dat file
             npts = min(mptsx,npts)
             phffo = zero
             rf2b  =  reff*reff*bohr
             do 420 i = 1, npts
                archi = achi(i) * cos(phchi(i))
                aichi = achi(i) * sin(phchi(i))
                arck  = dble(ck(i))
                aick  = dimag(ck(i))
                xlam  = 1.d10
                phff  = zero
                if (abs(aick).gt.eps) xlam= 1/aick
                if (achi(i).ge.eps) phff = atan2(aichi, archi)
c  remove 2 pi jumps in phases
                call pijump (phff, phffo)
                phffo  = phff
c  save values to arrays
                qfeff(i,ipth) = xk(i)/ bohr
                realp(i,ipth) = arck / bohr 
                xlamb(i,ipth) = max(xlmin, xlam) * bohr
                thepha(i,ipth)= phff
                thcaps(i,ipth)= 2 * dble(phc(i)) + l0*pi
                thsaps(i,ipth)= phff - thcaps(i,ipth)
                theamp(i,ipth)= achi(i)*xk(i)*rf2b*exp(2*reff/xlam)
 420         continue
c
c fill in the rest of qfeff so that it is monotonically increasing
             do 450  i = npts+1, mffpts
                if(qfeff(i,ipth).lt.qfeff(i-1,ipth)) then
                   qfeff(i,ipth) = 2*qfeff(i-1,ipth)-qfeff(i-2,ipth)
                   theamp(i,ipth)= zero
                end if
 450         continue
c
c if the next {feffil,iffrec} set has the same file name,
c read the next record
             if ((ipth.lt.mfffil) .and.
     $            (feffil(ipth+1).eq.feffil(ipth))) then
                ipth = ipth + 1
                go to  200
             end if
 500         continue
             close(iunit)
c  done reading feff.bin entry
          end if
c construct path id
          tmpstr = ''
 2505     format('reff=', f7.4, ', nlegs=',i2)
          write(tmpstr,2505) refpth(ipth),nlgpth(ipth)
          ilen  = istrln(tmpstr)
          tmpstr= tmpstr(1:ilen)//', path= '//at_symbol(izpth(0,ipth))

          ilen  = istrln(tmpstr)
          if (nlgpth(ipth) .eq. 2) then 
             tmpstr= tmpstr(1:ilen)//'<->'//at_symbol(izpth(1,ipth))
          else 
             do 2600 i = 1,  nlgpth(ipth)-1
                tmpstr= tmpstr(1:ilen)//'->'//at_symbol(izpth(i,ipth))
                ilen  = istrln(tmpstr)
 2600        continue 
             tmpstr= tmpstr(1:ilen)//'->'//at_symbol(izpth(0,ipth))
          endif
          ilen  = istrln(tmpstr)
          fpthid(ipth) = tmpstr(1:ilen)
c
          go to 50
       end if
       return
c end subroutine fefinp
       end




