       subroutine fitdat
c
c      this routine reads the input data files for feffit.
c
c    - if given, each data file will be opened and the chi(k)
c      data and documentation will be read with the routine inpdat.
c    - all the numbers for qmin, qmax, rmin, rmax, etc. are also
c      set here, using defaults from the values in the data files.
c    - as the data is read in, it is interpolated here onto a evenly
c      spaced k grid ( with dk = qgrid = 0.05).
c    - if backgound removal is requested, the number of knots in the
c      spline will be found and set here, rmin will be used as rbkg,
c      and the fit will be done on r=[0,rmax]. the variables for the
c      spline coeffs will be added to the end of the variable list,
c      so that the first nvuser variables will be those explicitly
c      "guess"ed, and the rest (numvar-nvuser) will be those implicitly
c      guessed by asking for background removal.
c    * the feffnnnn.dat files will be read in fefinp.
c
c      copyright 1993 university of washington         matt newville
c
c----------------------------------------------------------------------
c  common blocks for feffit
       include 'fitcom.h'

       character*10  ftype, frmtmp, strtmp*128
       character*100 docb(maxdoc)
       double precision qtemp(maxpts), chitmp(maxpts), rrmmax
       double precision qbtemp(maxpts), bkgtmp(maxpts), qrange
       double precision tmp1(maxpts), tmp2(maxpts), tmp3(maxpts)
       double precision drinfo(mdata) , xlog2, percnt, two, rsmall
       double precision drmin, q, qsmall, xexpo, dr_1k
       double precision dqmin, xqmax, xqmin
       integer       mftmin, id, ilen, istrln, isky, ist, ndocb
       integer       nexpo, nrmin, nrmax,  ntmp, ii, iv, imsg, ireclb
       integer       nbkgf, ipos, iposb, i,  nq1st, nqmin, nqmax
       parameter(xlog2 = 0.69314718056d0, percnt = 1.d-2, two = 2.d0)
       parameter(xqmin = 20.d0, xqmax = 35.d0)
c-----------------------------------------------------------------------
c   drmin is used as the spacing in r-space to use in the fit,
       drmin  = pi/( qgrid * mftfit)
c----------------------------------------------------------------------
c  open data files and get chi data
       do 500 id = 1, ndata
          ftype     = 'chi'
          nchi(id)  = maxpts
          irecl(id) = 512
          q1st(id)  = qgrid
          qlast(id) = 30.
          if (datain(id)) then
             ndoc(id) = maxdoc - 1
             call inpdat(ftype, frminp, chifil(id), vaxflg, skey(id),
     $            nkey(id), irecl(id), ndoc(id), doc(1,id), nchi(id),
     $            qtemp, chitmp, tmp1, tmp2, tmp3)
c
c  write output message that this file was successfully read
             ilen = max(1, istrln( chifil(id)))
             call smcase(frminp, 'a')
             messg = chifil(id)(1:ilen)
             strtmp = ' '
             if (frminp(1:2).eq.'uw') then
                if (skey(id).ne.' ') then
                   isky  = max(1, istrln(skey(id)))
                   if (nkey(id).ne.0) then
                      strtmp = ' , '//skey(id)(1:isky)
                   else
                      write (strtmp, 9010) nkey(id), skey(id)(1:isky)
                   endif
                elseif (nkey(id).ne.0) then
                   write (strtmp, 9015) nkey(id)
                endif
             endif
             ist = istrln(strtmp)
             if (ist.ge.1) call append(messg, ' '//strtmp(:ist) ,ilen)
             call echo( '        '//messg(1:ilen))
c
c  if a bkg(k) file was specified, subtract this from the chi(k) data
             if (bkgdat(id)) then
                ndocb = maxdoc - 1
                ftype      = 'chi'
                nbkgf      = maxpts
                ireclb     = irecl(id)
                frmtmp     = ' '
                call inpdat(ftype, frmtmp, bkgfil(id), vaxflg,
     $               skeyb(id), nkeyb(id), ireclb, ndocb, docb(1),
     $               nbkgf, qbtemp, bkgtmp, tmp1, tmp2, tmp3)
c
c  write output message that this file was successfully read
                ilen = max(1, istrln( bkgfil(id)))
                call smcase(frmtmp, 'a')
                messg = bkgfil(id)(1:ilen)
                strtmp = ' '
                if (frmtmp(1:2).eq.'uw') then
                   if (skeyb(id).ne.' ') then
                      isky  = max(1, istrln(skeyb(id)))
                      if (nkeyb(id).ne.0) then
                         strtmp = ' , '//skeyb(id)(1:isky)
                      else
                         write(strtmp,9010) nkeyb(id),
     $                        skeyb(id)(1:isky)
                      endif
                   elseif (nkeyb(id).ne.0) then
                      write(strtmp, 9015) nkeyb(id)
                   endif
                endif
                ist = istrln(strtmp)
                if (ist.ge.1)
     $               call append(messg,' '//strtmp(:ist),ilen)
                call echo( '              '//messg(1:ilen))
             end if
c  put chi data into single array, chiq, on qgrid spacing,
c  starting at q = qgrid : linear interpolation
             ipos      = 1
             iposb     = 1
             q1st(id)  = max(qgrid, qtemp(1))
             qlast(id) = qtemp(nchi(id))
             if ((q1st(id).ge.xqmin).or.(qlast(id).ge.xqmax)) then
                imsg = -1
                if (chkdat)  imsg = 1
                call finmsg(3010,' ',' ',imsg)
             end if
             do 150 i = 1, maxpts
                q = (i-1)*qgrid
                if ( (q.lt.q1st(id)).or.(q.gt.qlast(id))) then
                   chiq(i,id) = zero
                else
                   call qintrp( qtemp, chitmp, nchi(id), q,
     $                  ipos, chiq(i,id))
                   if (bkgdat(id)) then
                      call qintrp( qbtemp, bkgtmp, nbkgf, q,
     $                     iposb, bkgq(i,id))
                      chiq(i,id) = chiq(i,id) - bkgq(i,id)
                   end if
                end if
 150         continue
          end if
c
c  recover from incomplete inputs
          if (((outfil(id).eq.' ').or.(outfil(id).eq.'xxfitxx'))
     $        .and.(chifil(id).ne.' ')) outfil(id) = chifil(id)
c
c
c miscellaneous fiddling with input numbers:
c     find maximum path index
c     put q1st, qmin, qmax on qgrid, etc.
c     calculate number of points in various ranges
c-- put q1st, qmin, qmax on qgrid, get nqpts
          qsmall    = qgrid * percnt
c q1st
          nq1st     = int( (q1st(id) + qsmall) / qgrid)
          q1st(id)  = qgrid * nq1st
c qmin : make sure it is on the data range and larger than qgrid
          nqmin     = max(1, int( (qmin(id) + qsmall) / qgrid))
          qmin(id)  = max(qgrid * nqmin, q1st(id))
c qmax : truncate nqmax
          nqmax     = int( (qmax(id) + qsmall) / qgrid)
          qmax(id)  = qgrid * nqmax
          if (qmax(id).le.qmin(id))  then
             qmax(id) = qlast(id)
          else
             qmax(id) = min(qmax(id), qlast(id))
          end if
c nqpts, nqfit
          nqpts(id) = nqmax - nqmin + 1
          nqfit(id) = int( ( qlast(id) + qsmall + two) / qgrid )
cc          print*, ' mid fitdat:  '
cc          print*, q1st(1),qmin(1),qmax(1), qlast(1), nqpts(1), nqfit(1)
c
c-- r values (mostly in next loop)
          if (rmax(id).le.rmin(id))    rmax(id)  = rmin(id)
c
c define drinfo as spacing between independent points in r-space
c and keep smallest drinfo found so far
          drinfo(id) = pi / ( qmax(id) - qmin(id) )
          drmin      =  min(drmin, drinfo(id))
c
c done with this data set, for now
 500   continue
c----------------------------------------------------------------------
c  get mftfit (number of points for fit in fft range)
       rsmall = rgrid * 0.01d0
       do 700  id  = 1, ndata
          nrmin     = int( (rmin(id) + rsmall) / rgrid )  + 1
          nrmax     = int( (rmax(id) + rsmall) / rgrid )
          if (nrmax.le.nrmin) nrmax = nrmin + 1
          nrpts(id) = nrmax - nrmin + 1
          rmax(id)  = rgrid * nrmax
          rmin(id)  = rgrid * nrmin
          if (bkgfit(id)) then
             rbkg(id) = rgrid * nrmin
             rmin(id) = zero
          end if
          xinfo(id) = two * (one + ((rmax(id)-rmin(id)) / drinfo(id)))
c
c  if bkg removal is requested, figure out how many spline knots to
c  use, where to put them, and add these knots to the variable list.
          if (bkgfit(id)) then
c
c-bkg how many splines
             nbkg(id) = 1 + 2 * int( rbkg(id) / drinfo(id) )
             nbkg(id) = min(mtknot-korder-1, max(korder+1, nbkg(id)))
c-bkg where to put knots
             do 600 i = 1, korder
                qknot(i,id)          = qmin(id) - (korder-i) * qgrid
                qknot(nbkg(id)+i,id) = qmax(id) + (i-1) * qgrid
 600         continue
             qrange = qmax(id) - qmin(id)
             ntmp   = nbkg(id) - korder + 1
             do 620 i = korder+1, nbkg(id)
                qknot(i, id) = qmin(id) + (i-korder)*qrange/ntmp
 620         continue
c-bkg add spline coefs to the variable list, initialize to zero
             do 640  ii = 1, maxval
                if  (vnames(ii).eq.' ') go to 650
 640         continue
 650         continue
             iv = ii - 1
             do 680  i = 1, nbkg(id)
                numvar  = numvar + 1
                iv      = iv + 1
                if (numvar.gt.mvarys) call finmsg(3020,' ',' ',mvarys)
                if (iv.gt.maxval)     call finmsg(3040,' ',' ',maxval)
                xguess(numvar)= zero
                write(vnames(iv), 660)  '_bkg_#',id,'_',i
 660            format (a,i2.2,a,i2.2)
                icdval(1,iv)   = -1
 680         continue
          endif
c
c  figure out amount of information for this data set, and add to total
c  note: if rmin=0. we're doing bkg subtraction - there is only
c        one piece of information at r=0. at r > 0, the information
c        comes in pairs.
          xnidp  = xnidp + xinfo(id)
c
c finally, make the qwindo and rwindo (ie, the fft widnows)
c  for this data set.
c
cc          print*, ' call window'
          call window(sqwin(id), qwin1(id),
     $         qwin2(id), qmin(id), qmax(id), qgrid,
     $         maxpts, qwindo(1,id) )

          call window(srwin(id), rwin1(id),
     $         rwin2(id), rmin(id), rmax(id), rgrid,
     $         maxpts, rwindo(1,id) )
c
 700   continue
       if (xinfo(1).lt.two) xinfo(1) = two
       if (xnidp.lt.two)    xnidp    = two
       inform  = int(xnidp)
c
       xexpo  = log ( float (mftfit)) / xlog2
       nexpo  = min(11, max(8, nint(xexpo)) )
c
c make sure rlast is smaller than 10pi = pi/2/qgrid
       rrmmax = (pi/two) / qgrid
       if (rlast.gt.rrmmax) rlast = rrmmax

       call cffti(mftfit, wfftc)

cc       print*, ' end  of fitdat:  '
cc       print*, mftfit
cc       print*, ' q/r grid ', qgrid, rgrid
cc       print*, q1st(1), qmin(1), qmax(1), qlast(1), nqpts(1)
       return
c end subroutine fitdat
 9010  format (a, ' , ', i4, ' (', a,')' )
 9015  format (a, ' , ', i4 )
       end
