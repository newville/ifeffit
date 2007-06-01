       subroutine fitout
c
c   chi(k), chi(r), and chi(q) for data, theory, correction to
c   background and the contribution to the total theory from
c   each path, and written out. most of the code here is for
c   writing documents.
c
c      copyright 1993 university of washington         matt newville
c----------------------------------------------------------------------
       include 'fitcom.h'

       integer   lenfvc, istrln,  juser, j,labl, ntitle
       integer   jofl, ipre, ilen, ixs, iexist, im, iend, mftsav
       integer   i, mfit, id, ibscf, jtmp, idoc, j0, nqdata
       integer   jfeff, jtitle, ix, inpath, idpath, ititle
       integer   jdd, nstart, jlen
       double precision reff, degen, rsmall
       parameter(lenfvc = mdata*maxpts)
       character*128 outksp, outrsp, outenv, outpre, tmpstr*256
       character*10  skyk, skyr, skyq, sdelim
       character*100 outdoc(maxdoc)
       double precision  xbest(mvarys), temp(lenfvc)
       double precision  par(mpthpr),  qhi, decod, bvalue
       integer     imxpre
       external   istrln, bvalue, decod
       parameter (qhi = 20.d0, j0 = 0)
       parameter ( sdelim = ',})/]\\' )
c imxpre is one less than longest file name prefix
       imxpre  = 64
cc       print*, ' This is Fitout ', vaxflg, dosflg
       if (vaxflg)  imxpre = 28
       if (dosflg)  imxpre =  7
       do 10 i = 1, maxdoc
          outdoc(i) = ' '
 10    continue
c
       rsmall  = rgrid * 0.01d0
       mfit    = 0

       do 50 id = 1, ndata
          nrpts(id) = int( (rmax(id) - rmin(id) + rsmall) /rgrid) + 1
          mfit      = mfit + 2 * nrpts(id)
  50   continue
       mfit = min(mfit, lenfvc)
c  call fitfun to evaluate final version of best fit
       ifxvar  = 0
       do 70 i = 1, numvar
          xbest(i) = xfinal(i)
 70    continue
       call fitfun(mfit, numvar, xbest, temp, iend)
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  for each data set:
c       write out the data in k, r, and q space
c       write out the full model in k, r, and q space
c       write out the background in k, r, and q space
c       write out each path contribution in k, r, and q space
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       id = 1
 100   continue
c   construct output file names
         ilen = min(imxpre, max(1, istrln(outfil(id))))
c     find the last occurance of '.' that is *after* any
c     subdirectory delimeter (stored in sdelim)
         ipre = ilen
 122     continue
            ipre  = ipre - 1
            if (ipre.le.0) go to 124
            if (index(sdelim, outfil(id)(ipre:ipre)).ne.0)  then
               ipre = ilen + 1
               go to 124
            end if
            if (outfil(id)(ipre:ipre).ne.'.')  go to 122
 124     continue
         ipre = ipre - 1
         if (ipre.le.0) ipre  = ilen
c
         if (frmout.eq.' ') frmout = frminp
         if (frmout.eq.' ') frmout = 'ascii'
         outpre = outfil(id)(1:ipre)
         call smcase(frmout, 'a')
         if (frmout(1:2).eq.'uw') then
            outksp = outpre(1:ipre)//'.chi'
            outrsp = outpre(1:ipre)//'.rsp'
            outenv = outpre(1:ipre)//'.env'
         else
            outksp = outpre(1:ipre)//'k.xxx'
            outrsp = outpre(1:ipre)//'r.xxx'
            outenv = outpre(1:ipre)//'q.xxx'
         end if
         jofl = max(1, istrln(outksp))
c   write a message
         if ((.not.kspout).and.(.not.rspout)
     $                    .and.(.not.qspout)) then
            messg = 'no output files written ! '
            im    =  max(1, istrln(messg))
            go to 8000
         else
            messg = '          '
            im    = 10
            if (kspout) call append(messg,' '//outksp(:jofl)//' ',im)
            if (rspout) call append(messg,' '//outrsp(:jofl)//' ',im)
            if (qspout) call append(messg,' '//outenv(:jofl)//' ',im)
         end if
         call echo('      '//messg(:im))
c
c construct rwindo for output fft
         call window(srwin(id), rwin1(id),
     $        rwin2(id), rmin(id), rmax(id), rgrid,
     $        maxpts, rwindo(1,id) )
c
c  output for data chi
         iexist = 1
         if (datain(id)) then
            if (frmout(1:2).ne.'uw') then
               outksp = outpre(1:ipre)//'k.dat'
               outrsp = outpre(1:ipre)//'r.dat'
               outenv = outpre(1:ipre)//'q.dat'
            end if
            qlast(id) = min(qhi, qlast(id))
            skyk = ' '
            skyr = ' '
            skyq = ' '
            call xfsout(chiq(1,id),thiqr(1,id),frmout,vaxflg,
     $           outksp,outrsp,outenv,.false.,kspout,rspout,qspout,
     $           irecl(id),iexist,skyk,skyr,skyq,asccmt,mdocxx,
     $           qwindo(1,id),qweigh(id),rwindo(1,id),rweigh(id),
     $           wfftc,mftfit,rlast,q1st(id),qlast(id),
     $           pcout, qfeff(1,jffphs(id)),thepha(1,jffphs(id)), 
     $           mffpts,qgrid,ndoc(id),doc(1,id))
c     write a echoe
            if ((skyk.eq.' ').and.(skyr.eq.' ')
     $                       .and.(skyq.eq.' ')) then
               messg = 'data not written to output files'
            else
               messg = 'data written to'
               if (skyk.ne.' ') call append(messg,' chi(k) ',im)
               if (skyr.ne.' ') call append(messg,' chi(r) ',im)
               if (skyq.ne.' ')
     $              call append(messg,' filtered chi(k)',im)
            end if
         else
            messg = 'no data to write to output files'
         end if
         im = max(1, istrln(messg))
         call echo('           '// messg(:im))
c------------------------------------------------------------------
c  output for full theory chi
c
c  output documents
         call triml(titles(1,id))
         outdoc(1) = 'feffit result: '//titles(1,id)
         write(tmpstr,9280) qmin(id), qmax(id), qweigh(id),
     $        qwin1(id), qwin2(id), sqwin(id)(:12)
         outdoc(2) = tmpstr
         write(tmpstr,9301) rmin(id), rmax(id), xinfo(id)
         outdoc(3) = tmpstr
         if (datain(id)) then
             ixs = max(1, istrln(skey(id)))
             ix  = min(52-ixs, max(1, istrln(chifil(id))))
             write(outdoc(4),9360) skey(id)(1:ixs),chifil(id)(1:ix)
         else
             write(outdoc(4),9380)
         end if
c  jtitle gives the index for the next doc line to fill
         jtitle = 5
         do 220 ititle = 1, mtitle
            jlen = min(100, istrln( titles(ititle,id)))
            if (jlen.gt.0) then
               outdoc(jtitle) = '{ '//titles(ititle,id)(1:jlen)
               jtitle = jtitle + 1
            end if
 220     continue
c  get the rest documents from the first feffnnnn.dat used.
         idoc  = jtitle
         do 280 idpath = 1, mdpths
            inpath = jdtpth(idpath,id)
            if (inpath.gt.0) then
               jfeff = jpthff(inpath)
               jtmp  = 1
 240           continue
               if ( (idoc.le.maxdoc) .and. (jtmp.le.mffttl)) then
                  outdoc(idoc) = fefttl(jtmp, jfeff)
                  idoc = idoc + 1
                  jtmp = jtmp + 1
                  go to 240
               end if
               go to 285
            end if
 280     continue
 285     continue
c
c documents complete, so write out data :
         if (frmout(1:2).ne.'uw') then
            outksp = outpre(1:ipre)//'k.fit'
            outrsp = outpre(1:ipre)//'r.fit'
            outenv = outpre(1:ipre)//'q.fit'
         end if
         skyk = ' '
         skyr = ' '
         skyq = ' '
         call xfsout(thiq(1,id),thiqr(1,id),frmout,vaxflg,
     $        outksp,outrsp,outenv,kspcmp,kspout,rspout,qspout,
     $        irecl(id),iexist,skyk,skyr,skyq,asccmt,mdocxx,
     $        qwindo(1,id),qweigh(id),rwindo(1,id),rweigh(id),
     $        wfftc,mftfit,rlast,q1st(id),qlast(id),
     $        pcout, qfeff(1,jffphs(id)),thepha(1,jffphs(id)), 
     $        mffpts,qgrid,idoc,outdoc)
c     write a message
         if ((skyk.eq.' ').and.(skyr.eq.' ')
     $                    .and.(skyq.eq.' ')) then
            messg = 'full theory not written to output files'
            im    = istrln(messg)
         else
            messg = 'full theory written to'
            if (skyk.ne.' ') call append(messg,' chi(k) ',im)
            if (skyr.ne.' ') call append(messg,' chi(r) ',im)
            if (skyq.ne.' ')
     $           call append(messg,' filtered chi(k)',im)
         end if
         call echo('           '//messg(:im))
c------------------------------------------------------------------
c  outputs for background
         if (bkgout.and.(bkgfit(id).or.bkgdat(id)) ) then
c     ibscf holds place in xvar list of where the spline coefs
c     for the current data set are kept.
            ibscf  = nvuser+1
            if (id.gt.1) then
               do 290 jdd = 2, id
                  ibscf = ibscf + nbkg(jdd-1)
 290           continue
            endif
            nqdata = min(maxpts, max(2, nqfit(id)) + 10)
            if (bkgfit(id)) then
               do 300 i = 1, nqdata
                  thiq(i, id) =  bvalue(qknot(1,id), xfinal(ibscf),
     $                 nbkg(id), korder, qgrid*(i-1),j0)
 300           continue
            else
               do 320 i = 1, nqdata
                  thiq(i, id) = zero
 320           continue
            end if
c     construct documents for background
            write(outdoc(1),9410) rbkg(id), nbkg(id)
c
c documents complete, so write out data :
            if (frmout(1:2).ne.'uw') then
               outksp = outpre(1:ipre)//'k.bkg'
               outrsp = outpre(1:ipre)//'r.bkg'
               outenv = outpre(1:ipre)//'q.bkg'
            end if
            skyk = ' '
            skyr = ' '
            skyq = ' '
            call xfsout(thiq(1,id),thiqr(1,id),frmout,vaxflg,
     $           outksp,outrsp,outenv,.false.,kspout,rspout,qspout,
     $           irecl(id),iexist,skyk,skyr,skyq,asccmt,mdocxx,
     $           qwindo(1,id),qweigh(id),rwindo(1,id),rweigh(id),
     $           wfftc,mftfit,rlast,q1st(id),qlast(id),
     $           pcout, qfeff(1,jffphs(id)),thepha(1,jffphs(id)), 
     $           mffpts,qgrid,idoc,outdoc)
c     write a message
            if ((skyk.eq.' ').and.(skyr.eq.' ')
     $           .and.(skyq.eq.' ')) then
               messg = 'background not written to output files'
               im = max(1, istrln(messg))
            else
               messg = 'background written to'
               if (skyk.ne.' ') call append(messg,' chi(k) ',im)
               if (skyr.ne.' ') call append(messg,' chi(r) ',im)
               if (skyq.ne.' ')
     $              call append(messg,' filtered chi(k)',im)
            end if
            call echo('           '//messg(:im))
         elseif((.not.bkgout).and.(bkgdat(id).or.bkgfit(id))) then
            messg = 'background not written to output files'
            im = max(1, istrln(messg))
            call echo('           '//messg(:im))
         end if
c------------------------------------------------------------------
c  outputs for individual paths
c  optional outputs
         if (.not.allout)  then
            messg = 'not writing out data for individual paths'
            im = max(1, istrln(messg))
            call echo('           '//messg(:im))
         else
c-- for each path.................
            iexist = 0
            nqdata = min(maxpts, max(2, nqfit(id)) + 10)
            do 1000 idpath = 1, mdpths
               inpath    = jdtpth(idpath,id)
               if (inpath.eq.0)    go to 990
               jfeff     = jpthff(inpath)
               reff      = refpth(jfeff)
               degen     = degpth(jfeff)
               ixpath    = jfeff
               consts(4) = reff
c  evaluate the non-variable values
c   in case they depend on reff, local values to data set, etc
               nstart = nconst + numvar + 1
               call setval(nstart,nmathx,icdval,maxval,micode,
     $            consts,mconst,values,icdloc,mlocal,mdata,ixlocl,id)
c
c     evaluate the path parameters from the values and defaults
c    (these were found in fitfun before this routine was called)
               do 500 i = 1, mpthpr
                  par(i) = decod(icdpar(1, i, inpath), micode,
     $                 consts, values, defalt(i))
 500           continue
c  evaluate chi(k) for this path
               call chipth(theamp(1,jfeff), thepha(1,jfeff), 
     $ qfeff(1,jfeff),xlamb(1, jfeff), realp(1, jfeff), mffpts, 
     $ reff, degen, par(jps02), par(jpe0), par(jpei), par(jpdpha),
     $ par(jpdelr), par(jpsig2),par(jp3rd), par(jp4th), 
     $ tranq, rm2flg, nqdata, maxpts, thiqr(1,id), thiq(1,id))
c
c---- done evaluating theoretical chi for this path
c---  documents
               juser = jdtusr(idpath, id)
               ilen  = min(40, max(1, istrln( feffil(jfeff))))
               call triml(pthlab(inpath))
               labl = min(55, max(1, istrln( pthlab(inpath))))
               if (labl.le.0) then
                  if (iffrec(jfeff).eq.0) then
                     write(outdoc(1),9440) juser,feffil(jfeff)(1:ilen),
     $                    reff, degen, nlgpth(jfeff)
                  else
                     write(outdoc(1),9445) juser,feffil(jfeff)(1:ilen),
     $                iffrec(jfeff), reff, degen, nlgpth(jfeff)
                  end if
               else
                  write(outdoc(1),9460) juser,pthlab(inpath)(1:labl)
               end if
               write(outdoc(3), 9480) par(jpe0),par(jps02),
     $              par(jpdelr),par(jpsig2)
               if (dphflg) then
                  write(outdoc(4), 9540) par(jpei), par(jp3rd),
     $                 par(jp4th), par(jpdpha)
               else
                  write(outdoc(4), 9542) par(jpei), par(jp3rd),
     $                 par(jp4th)
               end if
               if (iffrec(jfeff).eq.0) then
                  write(outdoc(jtitle),9560) feffil(jfeff)(1:ilen)
               else
                  write(outdoc(jtitle),9565) feffil(jfeff)(1:ilen),
     $                 iffrec(jfeff)
               end if
               write(outdoc(jtitle + 1),9580) reff, degen
               ntitle  = jtitle + 2
c   write out path coordinates to titles
               do 600 i = 0, nlgpth(jfeff) - 1
                  j   = ntitle + i
                  if (i.eq.0) then
                     write(outdoc(j),9620) '>', ratpth(1,i,jfeff),
     $                    ratpth(2,i,jfeff), ratpth(3,i,jfeff),
     $                    iptpth(i,jfeff), izpth(i,jfeff)
                  else
                     write(outdoc(j),9640) '>', ratpth(1,i,jfeff),
     $                    ratpth(2,i,jfeff), ratpth(3,i,jfeff),
     $                    iptpth(i,jfeff), izpth(i,jfeff)
                  end if
 600           continue
c   fill in titles with those from feff file
               ntitle  = jtitle + 1 + nlgpth(jfeff)
               idoc    = maxdoc
               do 700 i = 1, mffttl
                  j   = ntitle + i
                  if(j.le.maxdoc)  outdoc(j) = fefttl (i, jfeff )
 700           continue
c     write data output for each path even if it is a repeat
               if (frmout(1:2).ne.'uw') then
                  if (juser.lt.1000) then
                     write(outksp(ipre+3:),740)  juser
                     write(outrsp(ipre+3:),740)  juser
                     write(outenv(ipre+3:),740)  juser
 740                 format (i3.3)
                  end if
               end if
               skyk = ' '
               skyr = ' '
               skyq = ' '
               call xfsout(thiq(1,id),thiqr(1,id),frmout,vaxflg,
     $              outksp,outrsp,outenv,kspcmp,kspout,rspout,qspout,
     $              irecl(id),iexist,skyk,skyr,skyq,asccmt,mdocxx,
     $              qwindo(1,id),qweigh(id),rwindo(1,id),rweigh(id),
     $              wfftc,mftfit,rlast,q1st(id),qlast(id),
     $              pcout, qfeff(1,jffphs(id)),thepha(1, jffphs(id)), 
     $              mffpts,qgrid,idoc,outdoc)
c     write a message
               if ((skyk.eq.' ').and.(skyr.eq.' ')
     $              .and.(skyq.eq.' ')) then
                  write(messg,9130)'path ',juser,' not written '
               else
                  write(messg,9130)'path ',juser,' written to '
                  if (skyk.ne.' ') call append(messg,' chi(k) ',im)
                  if (skyr.ne.' ') call append(messg,' chi(r) ',im)
                  if (skyq.ne.' ')
     $                 call append(messg,' filtered chi(k)',im)
               end if
               im = max(1, istrln(messg))
               call echo('           '//messg(:im))
 990           continue
 1000       continue
c-------  done looping through all the paths
c  repeat message saying which files were written
            messg = 'wrote files '
            im    = 10
            if (kspout) call append(messg,' '//outksp(:jofl)//' ',im)
            if (rspout) call append(messg,' '//outrsp(:jofl)//' ',im)
            if (qspout) call append(messg,' '//outenv(:jofl)//' ',im)
            call echo('        '//messg(:im))
c-------end
         end if
         id = id + 1
         if (id.le.ndata) go to 100
8000   continue
       return
c
c     formats
 9130  format(a,i5,a)
 9280  format ('k range = [',2f6.2,']; kw=',f4.1,
     $      '; dk1,dk2 =[',2f6.2,']; window=',a)
 9300  format ('r range = [',2f6.2,']; ',i3,
     $      ' independent points in data')
 9301  format ('r range = [',2f6.2,']; ',f6.2,
     $      ' independent points in data')
 9360  format ('fit to data: skey "',a,'" in file ', a)
 9380  format ('no data was used. feff files combined without fitting')
 9410  format('feffit background:  rbkg = ',f6.3, ' n_knots = ', i3)
 9440  format('> path',i6,': ',a,'; r=',f6.3,'; n=',f6.2,'; nlegs=',i3)
 9445  format('> path',i6,': ',a,',',i5,'; r=',f6.3,'; n=',f6.2,
     $      '; nlegs=',i3)
 9460  format('> path',i5,': ',a)
 9480  format('path  : e0 =',f8.4,'; amp =',f9.4,'; delr =',
     $      f9.5,'; sigma2 =',f10.6)
 9540  format('params: ei =',f8.4,'; 3rd =',f9.6,';  4th =',f9.6,
     $      '; dphase =',f10.6)
 9542  format('params: ei =',f8.4,'; 3rd =',f9.6,';  4th =',f9.6)
 9560  format('<<< feff data file used: ',a)
 9565  format('<<< feff data file used: ',a,',',i6)
 9580  format('path:   x         y         z   ipot iz ; r_eff =',
     $      f7.4,'; n_degen=',f6.2)
 9620  format(a,1x,f9.5,1x,f9.5,1x,f9.5,2x,i2,2x,i2,8x,'absorbing atom')
 9640  format(a,1x,f9.5,1x,f9.5,1x,f9.5,2x,i2,2x,i2)
c end subroutine fitout
       end
