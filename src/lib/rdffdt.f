       subroutine rdffdt(ffname,mtitle,mleg,mpts, ntitle, nleg, npts,
     $      title, reff, rwignr, degen, xyz, ipot, iz,
     $      qf,amplit,phase,cphase,sphase,xlamb,realp)
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
c
c  read a feffnnnn.dat file
       implicit none
       integer  i, ier1, ier2, ier3, ier4, ix, ileg, nwords
       integer      mtitle, mleg, mpts, ntitle, nleg, npts, istrln
       character*(*) ffname, title(mtitle), filnam*128
       integer       ipot(0:mleg), iz(0:mleg)
       double precision  reff, rwignr, degen, xyz(3,0:mleg)
       double precision  qf(*), amplit(*), phase(*)
       double precision  cphase(*), sphase(*)
       double precision  xlamb(*), realp(*), zero, xlmin

       character*40 stat*5, line*90, words(16), messg*80
       integer  iunit, iex, ierr
       parameter (zero = 0, xlmin = 1.d-8)
       double precision cdel, afeff,phfeff, redfac, xk, xlmda,  preal
       external istrln
       data  stat /'old'/

       iunit = 0
       xk    = zero
       filnam = ffname
       call openfl(iunit, filnam , stat, iex, ierr)
       if ((iex.lt.0).or.(ierr.lt.0))
     $      call warn(3, ' error reading file: '//filnam)
c  read top of feff.dat, keeping first mtitle comment lines
       ntitle = 0
 150   continue
       ntitle = ntitle + 1
       read(iunit,999) line
       call sclean(line)
       call triml(line)
       if (line(3:6) .eq. '----')  goto 200
       if (ntitle.le.mtitle) title(ntitle) = line
       go to 150
 200   continue
c   read and save reff and degen: feff version 5.03 and higher
       read(iunit,999) line
       call sclean(line)
       nwords = 4
       call bwords(line(2:), nwords, words)
       call str2in(words(1), nleg,   ier1)
       if (nleg .gt. mleg) then
          write(messg,'(2x,a,i2)')
     $         'too many legs in path. current limit is', mleg
          call echo(messg)
          call warn(2,'   '//filnam)
       end if
       call str2dp(words(2), degen,  ier2)
       call str2dp(words(3), reff,   ier3)
       call str2dp(words(4), rwignr, ier4)
       if ( (ier1.ne.0).or.(ier2.ne.0).or.(ier3.ne.0).or.
     $      (ier4.ne.0) )   then
          call warn(3, '   bad data in feffnnnn.dat file: '//filnam)
       end if
c   skip label and read and save path coordinates information
       read(iunit,999) line
       call sclean(line)
       nwords = 5
       do 300  ileg = 0, nleg - 1
          read(iunit,999) line
          call sclean(line)
          call bwords(line(2:), nwords, words)
          do 270 ix = 1, 3
             call str2dp( words(ix), xyz(ix,ileg),ierr )
 270      continue
          call str2in( words(4), ipot(ileg), ierr )
          call str2in( words(5), iz(ileg), ierr )
 300   continue
c      fill in last coordinate = first coordinate
       do 350  ix = 1, 3
          xyz(ix,nleg) = xyz(ix,0)
 350   continue
       iz(nleg)   = iz(0)
       ipot(nleg) = ipot(0)
c
c  skip one line then
c  read in q, amplit, phase, and real and imag parts of p
       read(iunit,999) line
       call sclean(line)
       nwords  = 7
       do 500 i = 1, mpts + 1
c
c          read(iunit,*, end = 505) xk, cdel, afeff, phfeff,
c     $        redfac, xlmda, preal

          if (i.gt.mpts) then
             call echo('  not enough memory for feff file: '//filnam)
             write(messg,'(2x,a,i3,a)') 'results above k = ',
     $            int(xk), ' will not be reliable'
             call warn(3, messg)
          end if

          read(iunit,999,end=505) line
          call sclean(line)
          if (istrln(line).ge.1)  then 
             call bwords(line, nwords, words)
             if (nwords .lt. 7) then
                call echo(' invalid feff file: '//filnam)
                go to 505
             end if
             call str2dp(words(1), qf(i),     ier1)
             call str2dp(words(2), cphase(i), ier1)
             call str2dp(words(3), amplit(i), ier1)
             call str2dp(words(4), sphase(i), ier1)
             call str2dp(words(5), redfac,    ier1)
             call str2dp(words(6), xlmda,     ier1)
             call str2dp(words(7), realp(i),  ier1)
             
             amplit(i) = amplit(i) * redfac
             xlamb(i)  = max(xlmin, xlmda)
          end if
 500   continue
 505   continue
       npts = i - 1
       close(iunit)
c
c  make sure no 2pi jumps in phase
       phase(1)  = cphase(1) + sphase(1)
       do 800  i = 2, npts
          call pijump ( cphase(i), cphase(i-1))
          call pijump ( sphase(i), sphase(i-1))
          phase(i)  = cphase(i) + sphase(i)
 800   continue
c
c  check that qf is monotonically increasing, filling in the high k
c  points if needed (feff provides monotonically increasing data, so the
c  first npts of qf are ok -- we just want to fill in the rest
c  of the points for later extrapolations)
       do 850  i = npts,  mpts
          if (qf(i).lt.qf(i-1)) then
             qf(i)=2*qf(i-1)-qf(i-2)
             amplit(i) = zero
             cphase(i) = zero
             sphase(i) = zero
             phase(i)  = zero
             realp(i)  = zero
             xlamb(i)  = 1.d10
          end if
 850   continue
c      done
       return
 999   format(a)
c end subroutine rdffdt
       end
       subroutine rdfb1(ffname,iunit,mtitle,mleg,mpts,npack,
     $      ntitle,npot,npts,rnrmav,l0,title,izpot,phc,ck,xk)
c
c  read top of feff.bin
       implicit none
       integer npot, npts, mtitle, mleg, mpts, iunit, ivers
       character*(*) ffname, filnam*128, title(mtitle), baddat*256
       integer izpot(0:mleg), l0, ntext,i, ntitle,ier1, ier2, ier3
       integer mptsx
       parameter(mptsx= 128)
       complex*16       phc(*), ck(*)
       double precision xk(*), ere(mptsx), rnrmav 
       integer  mwords, ierr, nwords, npack
       character*128 str
       parameter (mwords = 20 )
       character*30 words(mwords)
       if (npack.le.3) npack  = 8
       filnam = ffname
       baddat  = '   bad data in feff.bin file: '//filnam
 10    format(a)
c first line identifies file (only)
       read(iunit,10) str
       call sclean(str)
       if ((str(1:10).ne.'#_feff.bin'))  call echo(baddat)
       ivers = 1
ccc       if (str(1:14).eq.'#_feff.bin fil')   ivers = 1
       if (str(1:14).eq.'#_feff.bin v02')   ivers = 2
       
c second line contains ntitle, npot, npts
       read(iunit,10) str
       call sclean(str)
       if ((str(1:2).ne.'#_'))  call echo(baddat)
       nwords = 3
       call bwords(str(3:),nwords,words)
       if (nwords.ne.3)     call echo(baddat)
       call str2in(words(1), ntext, ier1)
       call str2in(words(2), npot,  ier2)
       call str2in(words(3), npts,  ier3)
       if ((ier1.ne.0).or.(ier2.ne.0).or.(ier3.ne.0))
     $      call echo(baddat)
c title lines
       ntitle = min(ntext,mtitle)
       do 20  i = 1, ntext
          read(iunit,10) str
          call sclean(str)
          if (str(1:2).ne.'#"')  call echo(baddat)
          if (i.le.ntitle) title(i) = str(3:)
 20    continue
c line with several numbers, only rnrmav and l0 are needed for exafs
       read(iunit,10) str
       call sclean(str)
       if (str(1:2).ne.'#&')   call echo(baddat)
       nwords = 8
       call bwords(str(3:),nwords,words)
       if (ivers.eq.1) then 
           if (nwords.ne.8)   call echo(baddat)
           call str2dp(words(3), rnrmav, ier1)
           call str2in(words(8), l0, ier2)
           if ((ier1.ne.0).or.(ier2.ne.0))  call echo(baddat)
        elseif (ivers.eq.2) then 
           if (nwords.ne.5)   call echo(baddat)
           call str2dp(words(2), rnrmav, ierr)
           call str2in(words(5), l0, ier2)
           if ((ier1.ne.0).or.(ier2.ne.0))  call echo(baddat)
        end if
c read pot labels and atomic numbers
       read(iunit,10) str 
       call sclean(str)
       if (str(1:2).ne.'#@')   call echo(baddat)
       nwords = min(mwords, 2 * npot + 2 )
       call bwords(str(3:), nwords, words)
       if (nwords.ne.(2 + 2*npot))  call echo(baddat)
       do 30 i = 0, npot
          call str2in(words(2+npot+i),izpot(i),ier1)
          if (ier1.ne.0)    call echo(baddat)
 30    continue 
c
c read packed arrays 
       call rdpadc(iunit,npack,phc, npts)
       call rdpadr(iunit,npack,ere, npts)
       call rdpadc(iunit,npack,ck,  npts)
       call rdpadr(iunit,npack,xk,  npts)
c done
       return
       end


