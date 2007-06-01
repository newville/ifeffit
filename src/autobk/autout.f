       subroutine autout
c
c  matt:  add preout to write pre-edge subtracted xmu and bkg
c   data outputs for autobk:
c   uses the routine outdat to write to uwexafs
c   or column files, as given by frmout.
c
c   copyright 1992  university of washington :          matt newville
c--------------------------------------------------------------------
       include 'autobk.h'
c--------------------------------------------------------------------
c local variables
       integer  istrln, ix, i, it, ixf
       integer  iskxm, iskth, ndoc, jnew, nkyout, ipos, iexist
       double precision  e0, qlast, q1st
       double precision  rmin, energ, q, rlast, outstp
       double precision  tmp1(maxpts), tmp2(maxpts), tmp3(maxpts)
       double precision  enout(maxpts), tmp4(maxpts)
       character*128 outdoc(maxdoc)
       character*128  outksp, outrsp, outbkg, outpre
       character*10  skyksp, skyrsp, skybkg, skypre, ftype
       logical   chiksp
       data      chiksp , iexist , rmin/.true., 1 , zero /
c--------------------------------------------------------------------
cc       print*,'entered autout'
c  get some preliminary stuff
       it     = 1
       rlast  = 10.d0
       e0     = ee + e0shft
       qlast  = qgrid * (int(sqrt((emax - e0)*etok) / qgrid ) - 1 )
       nqpts  = int ( qlast / qgrid )
       q1st   = min(qgrid,qmin)
       mftfit  = 2048
       call cffti(mftfit, wfftc)

c
c  determine output format
        if (frmout.eq.' ') then
           if (frminp.eq.' ')  frminp = 'ascii'
           frmout = frminp
        end if 
       call smcase(frmout, 'a')
c----output file names: iodot was found in autinp
       if (frmout(1:2).eq.'uw') then
          outksp = chif(1:iodot)//'.chi'
          outbkg = chif(1:iodot)//'.bkg'
          outrsp = chif(1:iodot)//'.rsp'
          outpre = chif(1:iodot)//'.xmu'
       else
          outpre = chif(1:iodot)//'e.xmu'
          outbkg = chif(1:iodot)//'e.bkg'
          outksp = chif(1:iodot)//'k.chi'
          outrsp = chif(1:iodot)//'r.chi'
       end if
       ix = max(1, istrln(outksp))
       do 80 i = 1, maxpts
          tmp1(i) = zero  
          tmp2(i) = zero  
          tmp3(i) = zero  
          tmp4(i) = zero  
 80    continue
       if (eshout) then
          do 90 i = 1, maxpts
             enout(i) = energy(i) - e0
 90       continue 
       else
          do 95 i = 1, maxpts
             enout(i) = energy(i)
 95       continue 
       end if
c  write documentaion for data and background
       write(outdoc(1), 9000) 'data  : ', commnt(1:65)
       iskxm = max(1, istrln(skeyxm))
       iskth = max(1, istrln(skeyth))
       ixf   = min(74, max(1, istrln(xmuf)))
       write(outdoc(2), 9005) skeyxm(1:iskxm), xmuf(1:ixf)
       if (theory) then
          it  = min(74, max(1, istrln(theorf)))
          write(outdoc(3), 9010) skeyth(1:iskth), theorf(1:it)
       else
          outdoc(3) = '    using simple minimization '
       end if
c pre-edge info
       write(outdoc(4), 9020)  e0, predg1, predg2, step
c fourier window type
       if (iwindo.eq.1) then
          write(outdoc(5), 9030)  qmin, qmax, qweigh,
     $         '; hanning fraction =', windo1
       elseif (iwindo.eq.2) then
          write(outdoc(5), 9030)  qmin, qmax, qweigh,
     $         '; gaussian dk      =', windo1
       elseif (iwindo.eq.3) then
          write(outdoc(5), 9030)  qmin, qmax, qweigh,
     $         '; lorentzian dk    =', windo1
       elseif (iwindo.eq.4) then
          write(outdoc(5), 9035)  qmin, qmax, qweigh,
     $         '; parzen dk1, dk2  =', windo1, windo2
       elseif (iwindo.eq.5) then
          write(outdoc(5), 9035)  qmin, qmax, qweigh,
     $         '; welch  dk1, dk2  =', windo1, windo2
       else
          write(outdoc(5), 9035)  qmin, qmax, qweigh,
     $         '; sills  dk1, dk2  =', windo1, windo2
       end if
c
       if (theory) then
          write(outdoc(6), 9040) rmin,rbkg,rbkg,r1st,nsplin
       else
          write(outdoc(6), 9045) rmin,rbkg,nsplin
       end if
c--add previous document at end
       ndoc   = 6
       jnew   = 0
150    continue 
         jnew  = jnew + 1
         if (jnew.gt.maxdoc) go to 155
         call triml(xmudoc(jnew))
         if ( (ndoc.lt.19).and.(xmudoc(jnew).ne.' ') )  then
            ndoc  = ndoc + 1
            outdoc(ndoc) = xmudoc(jnew)
            go to 150
         end if
155    continue
c       print*,' outdoc(5)(1:20)=',outdoc(5)(1:20)
c----write out pre-edge subtracted xmu(e) (if preout = t)
       outstp = one
       if (preout) then
          if (nrmout) outstp = step
          outdoc(2)(1:3) = 'xmu'
          ftype = 'xmu'
          do 300 i = 1, nxmu
             tmp1(i) = xmudat(i) / outstp
 300      continue
          call outdat( ftype, frmout, outpre, vaxflg, asccmt, skypre,
     $         nkyout, irecl, ndoc, mdocxx, outdoc,  nxmu , enout,
     $         tmp1,  tmp2,  tmp3, tmp4, iexist)
          if (skypre.ne.' ') then
            call echo('           pre-edge subtracted xmu(e)'//
     $            ' written to: '// outpre(1:ix) )
          else
            call echo('           pre-edge subtracted xmu(e)'//
     $            ' already in: '// outpre(1:ix) )
          end if
          outdoc(2)(1:3) = 'chi'
       end if
c----write out chi(k) and chi(r) for data :
       skyrsp = ' '
       skyksp = ' '
       call chiout(chiq, outksp, outrsp, frmout, vaxflg, 
     $      chiksp, chirsp, irecl, ndoc, outdoc, q1st, 
     $      qlast, qgrid, qweigh, windo, wfftc, mftfit, 
     $      rlast, asccmt, mdocxx, iexist, skyksp, skyrsp)
       if (skyksp.ne.' ') then
          call echo('           chi(k) for data written to: '//
     $         outksp(1:ix) )
       else
          call echo('           chi(k) for data already in: '//
     $         outksp(1:ix) )
       end if
       if (skyrsp.ne.' ') then
          call echo('           chi(R) for data written to: '//
     $         outrsp(1:ix) )
       elseif (chirsp) then
          call echo('           chi(R) for data already in: '//
     $         outrsp(1:ix) )
       end if
c----write out background(e)
       if (bkgxmu) then
          write(outdoc(1), 9000) 'bckgrd: ', commnt(1:65)
          outdoc(2)(1:3) = 'bkg'
          ftype = 'xmu'
c
c  decide if pre-edge should be undone or not
          if (preout) then
             do 400 i = 1, nxmu
                tmp1(i) = spline(i) / outstp
 400         continue
          else
             do 405 i = 1, nxmu
                tmp1(i) = spline(i) + bpre + slopre*energy(i)
 405         continue
          endif
          call outdat( ftype, frmout, outbkg, vaxflg, asccmt, skybkg, 
     $         nkyout, irecl,  ndoc, mdocxx, outdoc,  nxmu , enout,
     $         tmp1,  tmp2,  tmp3, tmp4, iexist)
          if (skybkg.ne.' ') then
            call echo('           background(e) written to: '//
     $                  outbkg(1:ix) )
          else
            call echo('           background(e) already in: '//
     $                  outbkg(1:ix) )
          end if
       end if
c----write out background(k)
       if (bkgchi) then
          write(outdoc(1), 9000) 'bckgrd: ', commnt(1:65)
          do 480 i = 1, nxmu
             energ = energy(i) - e0
             if (energ.lt.0) then
                tmp3(i)  = - sqrt( - etok*energ)
             else
                tmp3(i)  =   sqrt(   etok*energ)
             end if
480       continue
c   write background(k) to temporary file, write it  out
          ipos = 1
           do 500 i = 1, nqpts+10
             q = (i-1)*qgrid  
             if ((q.ge.q1st).and.(q.le.qlast)) then
                call lintrp(tmp3, spline, nxmu, q, ipos, tmp4(i) )
             else
                tmp4(i) = zero
             end if  
500       continue
c                               file name
          if (frmout(1:2).ne.'uw') then
             outksp = chif(1:iodot)//'k.bkg'
             outrsp = chif(1:iodot)//'r.bkg'
          end if
          skyksp = ' '
          call chiout(tmp4, outksp, outrsp, frmout, vaxflg, 
     $         bkgchi, bkgrsp, irecl, ndoc, outdoc, q1st, 
     $         qlast, qgrid, qweigh, windo, wfftc, mftfit, 
     $      rlast, asccmt, mdocxx, iexist, skyksp, skyrsp)
          if (skyksp.ne.' ') then
            call echo('           background(k) written to: '//
     $                  outksp(1:ix) )
          else
            call echo('           background(k) already in: '//
     $                  outksp(1:ix) )
          end if
       end if
c----documents for theoretical standard
       if ( theory.and.(thersp.or.thechi)) then
          write(outdoc(1), 9000) 'theory: ', commnt(1:65)
          write(outdoc(2), 9100)  skeyxm(1:iskxm), xmuf(1:ixf)
          write(outdoc(3), 9110)  skeyth(1:iskth), theorf(1:it)
          write(outdoc(4), 9120)  rbkg, r1st, theamp
c--   add previous document at end
         ndoc   = 4
         jnew   = 0
610      continue 
           jnew  = jnew + 1
           if (jnew.gt.maxdoc) go to 615 
           call triml(thedoc(jnew))
           if ( (ndoc.lt.19).and.(thedoc(jnew).ne.' ') )  then
              ndoc  = ndoc + 1
              outdoc(ndoc) = thedoc(jnew)
              go to 610
           end if
615      continue
         if (ndoc.le.19) then 
            do 620 i = ndoc, 19
              outdoc(i) = ' '
620         continue
         end if
c----evaluate chi(k) for theory
         do 700 i = 1, maxpts
             q       = i*qgrid
             tmp3(i) = theamp*thiq(i)
             tmp4(i) = zero 
700      continue
c----write out chi(k) and chi(r) for theory
c                               file name
          if (frmout(1:2).ne.'uw') then 
             outksp = chif(1:iodot)//'k.stn'
             outrsp = chif(1:iodot)//'r.stn'
          end if
          skyksp = ' '
          skyrsp = ' '
          call chiout(tmp3, outksp, outrsp, frmout, vaxflg,
     $         thechi, thersp, irecl, ndoc, outdoc, q1st,
     $         qlast, qgrid, qweigh,  windo,  wfftc, mftfit, 
     $         rlast, asccmt, mdocxx, iexist, skyksp, skyrsp)
          if (skyksp.ne.' ') then
            call echo('           chi(k) for theory written to: '//
     $                  outksp(1:ix) )
          else
            call echo('           chi(k) for theory already in: '//
     $                  outksp(1:ix) )
          end if
          if (skyrsp.ne.' ') then
            call echo('           chi(R) for theory written to: '//
     $                  outrsp(1:ix) )
          elseif (thersp) then
            call echo('           chi(R) for theory already in: '//
     $                  outrsp(1:ix) )
          end if
       end if
c----all done
       call echo('   ----------------------------------'//
     $            '----------------------------------')
       return
c format statements
 9000  format(2a)
 9005  format('chi: from skey ',a,' of ',a)
 9010  format('    using skey ',a,' of ',a)
 9020  format(' e0 =',f9.2,'; pre-edge range =[', 2f8.1,
     $      ']; edge step =', f7.3 )
 9030  format(' k range =[',2f6.2,']; k weight =',f6.2,a20,f5.3)
 9035  format(' k range=[',2f6.2,']; k weight=',f6.2,a20,2f6.2)
 9040  format(' bkg r =[',2f6.2,']; 1st shell r =[',2f6.2,']; ',
     $         i3, ' knots in spline')
 9045  format(' bkg r =[',2f6.2,'];  ',i3,' knots in spline')

 9100  format('chi: from skey ',a,' of ',a)
 9110  format('    using skey ',a,' of ',a)
 9120  format('1st shell fit range=[',2f6.2,'] =>  amp-scale =',f8.5)

c end subroutine autout
       end
