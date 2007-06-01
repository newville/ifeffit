       subroutine fefinp(mpts, mfil, mtitle, mleg,
     $      title, feffil,  iffrec, degflg, degpth,
     $      refpth, rwgpth, ratpth, theamp,
     $      thepha, qfeff, cphase, sphase,
     $      realp,  xlamb, nlgpth,  izpth, iptpth)
c
c    read path information from feff files:
c    readse either feffnnnn.dat files (feff5 or higher) or the
c    ascii feff.bin files from feff702
c
c    copyright 1996        matt newville
       implicit none
       integer   mpts, mfil, mtitle, mleg, mdocx, npack
       double precision degpth(mfil), refpth(mfil), rwgpth(mfil)
       double precision ratpth(3, 0:mleg, mfil), qfeff(mpts, mfil)
       double precision theamp(mpts, mfil), thepha(mpts, mfil)
       double precision cphase(mpts, mfil), sphase(mpts, mfil)
       double precision realp( mpts, mfil), xlamb( mpts, mfil)
       integer  nlgpth(mfil), iffrec(mfil)
       integer  izpth(0:mleg, mfil), iptpth(0:mleg, mfil)
       character*128 title(mtitle, mfil), feffil(mfil)

       integer   mlegx, mptsx, mwords, j, nleg, itmp, nepts,i
       integer   nwords,l0,ndoc,npot, ilen, ipth, istrln
       integer   nunit, iex, npts, ierr, ntitle, iunit
       parameter (mlegx = 10, mptsx= 100, mwords = 30, mdocx=10)
       character*64  filnam*128, stat*10, str*128, words(mwords)
       character*128 doc(mdocx), messg
       logical          degflg, exist
       integer          izpot(0:mlegx)
       complex*16       phc(mptsx), ck(mptsx), coni, cchi
       double precision xk(mptsx), achi(mptsx), phchi(mptsx)
       double precision beta(mlegx), rf2b, rnrmav
       double precision eps, phff, phffo, xlam, reff, bohr, zero
       parameter (zero = 0.d0, eps = 1.d-12, bohr = 0.529 177 249d0)
       parameter (coni = (0.d0,1.d0))
       parameter (stat = 'old')
       external   istrln
       npack = 8
c  feff.dat data extraction
c  loop to get data from at most maxpth paths
       ipth = 0
  50   continue
       ipth = ipth + 1
c   skip unused paths
       if ((ipth.le.mfil).and.(feffil(ipth).ne.' ')) then
c   get next file name from feffil, check that it exists
          exist  = .false.
          filnam = feffil(ipth)
          call triml (filnam)
          ilen = max(1, istrln(filnam))
          inquire ( file = filnam,  exist = exist)
c  failed to find this file: stop with warning message
          if (.not.exist) call finmsg(1001,filnam, ' ',0)
c  read data from file
          if (iffrec(ipth).eq.0) then
c feff5-style feffnnnn.dat file
             call echo( '        '//filnam(1:ilen))
ccc             print*, 'FEFINP:  call rdffdt '
             call rdffdt(filnam,mtitle,mleg,mpts,ntitle,
     $            nlgpth(ipth), npts,title(1,ipth),
     $            refpth(ipth), rwgpth(ipth),
     $            degpth(ipth), ratpth(1,0,ipth),
     $            iptpth(0,ipth), izpth(0,ipth),
     $            qfeff(1,ipth), theamp(1,ipth),
     $            thepha(1,ipth), cphase(1,ipth), sphase(1,ipth),
     $            xlamb(1,ipth), realp(1,ipth))
cc             print*, '    ', refpth(ipth), degpth(ipth), ipth
cc             print*, 'FEFINP q  ', qfeff(1,ipth), qfeff(2,ipth),
cc     $            qfeff(3,ipth)
             if (.not.degflg) degpth(ipth) = 1
          elseif (iffrec(ipth).ne.0) then
c feff7-style feff.bin
             iunit = 0
             call openfl(iunit,filnam,stat,iex,ierr)
             if ((iex.lt.0).or.(ierr.lt.0))
     $            call finmsg(1003,filnam, ' ',0)
c read top of feff.bin for all the records from this file
             call rdfb1(filnam,iunit,mdocx,mlegx,mptsx, npack,
     $            ndoc, npot,
     $            npts, rnrmav, l0, doc,izpot,phc,ck,xk)
c skip to next record within this file
 120         continue
             read(iunit,'(a)',end=500) str
             call sclean(str)
             call triml(str)
             if (str(1:2).ne.'##')   go to 120
             nwords = 1
             call bwords(str(3:),nwords,words)
             call str2in(words(1) , itmp, ierr)
c  skip this record (and go to next) if this isn't the record we want
             if (itmp.ne.iffrec(ipth)) go to 120
c we found the right record:  read in path information
             ilen = istrln(filnam)
             write(messg,'(a,1x,a,1x,i5)')
     $            filnam(1:ilen), ',', iffrec(ipth)
             ilen = istrln(messg)
             call echo( '              '//messg(1:ilen))
             rwgpth(ipth) = rnrmav
             do 145 i = 1, ndoc
                title(i,ipth) =  doc(i)
 145         continue
             nwords = mwords
             call bwords(str(3:),nwords,words)
             call str2in(words(2), nleg, ierr)
             nlgpth(ipth) = nleg
             call str2dp(words(3), degpth(ipth), ierr)
             call str2in(words(4), nepts, ierr)
             call str2dp(words(5), refpth(ipth), ierr)
             reff  = refpth(ipth) / bohr
             do 170 j = 1, nleg
                call str2in(words(6+j),i,ierr)
                iptpth(j,ipth) = i
                izpth(j,ipth) = izpot(i)
 170         continue
             iptpth(0,ipth) = 0
             izpth(0,ipth)  = izpot(0)
             call rdpadr(iunit,npack,ratpth(1,1,ipth),3*nleg)
             do 190 i = 0, nleg
                do 185 j = 1, 3
                   ratpth(j,i,ipth) = ratpth(j,i,ipth) * bohr
 185            continue
 190         continue
c  note that we really don't care about beta, eta, ri arrays, so we'll
c  just skip them here:
             call rdpadr(iunit,npack,beta,nleg)
             call rdpadr(iunit,npack,beta,nleg)
             call rdpadr(iunit,npack,beta,nleg)
c but we really want these arrays (amplitude and phase)
             call rdpadr(iunit,npack,achi,nepts)
             call rdpadr(iunit,npack,phchi,nepts)
             do 230 j = nepts+1, mptsx
                achi(j)  = zero
                phchi(j) = zero
 230         continue
c now convert this into same info as in feff.dat file
             npts = min(mptsx,npts,mpts)
             phffo = zero
             rf2b  =  reff*reff*bohr
             do 300 i = 1, npts
                cchi = achi(i) * exp (coni*phchi(i))
                if (abs(dimag(ck(i))) .gt. eps) then
                   xlam= 1/dimag(ck(i))
                else
                   xlam = 1.d10
                end if
                if (abs(cchi).ge.eps) then
                   phff = atan2 (dimag(cchi), dble(cchi))
                else
                   phff = zero
                end if
c  remove 2 pi jumps in phases
                call pijump (phff, phffo)
                phffo  = phff
c  save values to arrays
                qfeff(i,ipth) = xk(i) / bohr
                realp(i,ipth) = dble ( ck(i) / bohr )
                xlamb(i,ipth) = xlam  * bohr
                thepha(i,ipth)= phff
                theamp(i,ipth)= abs(cchi*xk(i)*exp(2*reff/xlam))*rf2b
 300         continue
c
c fill in the rest of qfeff so that it is monotonically increasing
             do 350  i = npts+1, mpts
                if(qfeff(i,ipth).lt.qfeff(i-1,ipth)) then
                   qfeff(i,ipth) = 2*qfeff(i-1,ipth)-qfeff(i-2,ipth)
                   theamp(i,ipth)= zero
                end if
 350         continue
c
c if the next {feffil,iffrec} set has the same file name,
c read the next record
             if ((ipth.lt.mfil) .and.
     $            (feffil(ipth+1).eq.feffil(ipth))) then
                ipth = ipth + 1
                go to  120
             end if
 500         continue
             close(iunit)
c  done reading feff.bin entry
          end if
          go to 50
c
       end if
       return
 998   format(a1,a)
 999   format(a)
c end subroutine fefinp
       end
