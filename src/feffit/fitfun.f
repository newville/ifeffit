       subroutine fitfun(mvec, nvar, xvar, fvec, iend)
c
c  evaluate function to minimize in the least squares sense by lmdif.
c  the function fvec is the difference between chi(r) for data and
c  modelled theory on the fit range [rmin, rmax]. the variables
c  modify the theory chi(k), evaluated as a sum over paths, using
c  routine chipth for the chi(k) for each path.
c
c     copyright 1993   university of washington    matthew newville
c
c arguments:
c   mvec    number of evaluations of fvec ( = 2*nrpts )    (in)
c   nvar    number of variables xvar      ( = numvar  )    (in)
c   xvar    array of variables                             (in)
c   fvec    function of the variables to minimize          (out)
c   iend    integer stopping flag (not currently used)     (in)
c
c note: since this routine (and the routines it calls) is called
c       so often by lmdif (and fdjac2) efficiency and speed are
c       important. (usually clarity is more important than speed.).
c       so this routine is a bit terse.
c----------------------------------------------------------------------
       include 'fitcom.h'

c  local variables
c  warning: do not dimension the array xvar as "xvar(nvar)" !
c           this routine may be called with nvar = 0,
c           and an array dimension 0 is not allowed.
       integer   lenfvc, mvec, nvar, iend, id, i
       integer   nstart, jfit, nfit, nfit1
       integer   j0, nqdata, ibscf, jdd, inpath, idpath, jfeff
       parameter(lenfvc = mdata * maxpts)
       double precision reff, degen, xolow, xohigh
       double precision tchiqi(maxpts), tchiqr(maxpts)
       double precision thifit(maxpts)
       double precision xvar(mvarys), fvec(lenfvc), par(mpthpr)
       double precision rfact, bvalue, decod
       external  rfact, bvalue, decod
       j0  =  0
c----------------------------------------------------------------------
cc       print*, ' fitfun   :: final = ', final
c  use the values of the variables to evaluate all the "values"
c  for both the variables and the user-defined functions
c  note: values(i) is a variable       if  icdval(1,i) < 0
c        values(i) is a user function  if  icdval(1,i) > 0
c        values(i) is unused           if  icdval(1,i) = 0
       id    = 1
       do 20 i = 1, nvar
          values(i) = xvar(i)
 20    continue
cc       print*, ' vars: ', xvar(1), xvar(2), xvar(3), nvar

c don't need to evaluate the obvious constants, so we can start the
c looping here at nconst+nvar (see fitchk and fixicd)
       nstart = nconst + nvar + 1
       
       call setval(nstart,nmathx,icdval,maxval,micode,
     $      consts,mconst,values,icdloc,mlocal,mdata,ixlocl,id)
c
c  sum function to minimize over data sets
c   jfit is the counter (through all the data sets)
c   for the total number of fitting points
       jfit = 0
       do 3000 id = 1, ndata
          nqdata = min(maxpts, max(2, nqfit(id)) + 10)
          if (ifft(id).eq.1) then
             xolow  = rmin(id)
             xohigh = rmax(id)
             nfit   = 2 * max (1, nrpts(id))
          elseif (ifft(id).eq.2) then
             xolow  = qmin(id)
             xohigh = qmax(id)
             nfit   = 2 * max (1, nqpts(id))
          else
             xolow  = qmin(id)
             xohigh = qmax(id)
             nfit   = 2 * max (1, nqpts(id))
          endif
          do 200 i = 1, nfit
             thifit(i)   = zero
 200      continue
c  re-initialize array for theoretical chi(k)
c  by assigning this to the background function
cc          print*, 'FITFUN B ', id, xolow, xohigh, nfit
          do 300 i = 1, nqdata
             thiq(i, id) = zero
             tchiqi(i)   = zero
             tchiqr(i)   = zero
             if (final) thiqr(i, id) = zero
             if ( bkgfit(id))  then
c  ibscf holds place in xvar list of where the
c  spline coefs for the current data set are kept.
                ibscf  = nvuser+1
                if (id.gt.1) then
                   do 290 jdd = 2, id
                      ibscf = ibscf + nbkg(jdd-1)
 290               continue
                endif
                thiq(i, id) = thiq(i, id) +
     $               bvalue(qknot(1,id), xvar(ibscf),
     $               nbkg(id),korder,qgrid*(i-1),j0)
             end if
 300      continue
c
c   sum over paths for theory chi for this data set
          do 1000 idpath = 1, mdpths
             inpath    = jdtpth(idpath,id)
             if (inpath.le.0)    go to 990
             jfeff     = jpthff(inpath)
             reff      = refpth(jfeff)
             degen     = degpth(jfeff)
             ixpath    = jfeff
             consts(4) = reff
             consts(5) = degen
cc             print*, ' FITFUN: path ', idpath, jfeff, reff, degen
c  evaluate the non-variable values
             call setval(nstart,nmathx,icdval,maxval,micode,
     $            consts,mconst,values,icdloc,mlocal,mdata,ixlocl,id)
c  evaluate the path parameters from "values"
             do 500 i = 1, mpthpr
                par(i) = decod(icdpar(1, i, inpath), micode,
     $                         consts, values, defalt(i))
 500         continue
c  get chi(k) for this path from feff and path parameters
             if ( (inpath.gt.0).and.(jfeff.gt.0))  then
                if (iprint.ge.3) then
                   write(ifxvar,*) 'calling chipth:'
                   write(ifxvar,*) 'nfit, jfeff = ',nfit, jfeff
                   write(ifxvar,*) 'nqdata, maxpts = ',nqdata, maxpts
                   write(ifxvar,*) 'reff, mffpts = ', reff, mffpts
                   write(ifxvar,*) ' theamp(28,jfeff),thepha(28,jfeff)'
                   write(ifxvar,*) theamp(28,jfeff), thepha(28,jfeff)
                   write(ifxvar,*) ' qfeff(28,jfeff) =',qfeff(28,jfeff)
                   write(ifxvar,*) ' xlamb(28,jfeff), realp(28,jfeff)'
                   write(ifxvar,*) xlamb(28,jfeff), realp(28,jfeff)
                   write(ifxvar,*) ' reff, degen = ', reff, degen
                end if
cc                print*, ' id e0 ', id, par(jpe0)
                call chipth(theamp(1,jfeff), thepha(1,jfeff),
     $  qfeff(1,jfeff), xlamb(1,jfeff), realp(1,jfeff), mffpts,
     $  reff, degen, par(jps02),  par(jpe0),  par(jpei), par(jpdpha),
     $  par(jpdelr), par(jpsig2), par(jp3rd), par(jp4th), tranq, 
     $  rm2flg, nqdata, maxpts, tchiqr, tchiqi)

                if (iprint.ge.3) then
                   write(ifxvar,*) 'called chipth: jfeff, id = ',
     $                  jfeff,id
                   write(ifxvar,*) ' degen, s02, e0, ei = ',degen,
     $                  par(jps02),par(jpe0),par(jpei)
                   write(ifxvar,*) ' dphas,delr, sig2 = ',
     $                  par(jpdpha),par(jpdelr),par(jpsig2)
                   write(ifxvar,*) 'nqdata, maxpts = ',nqdata, maxpts
                   write(ifxvar,*) 'tchiqr(1),tchiqi(1)'
                   write(ifxvar,*)  tchiqr(1),tchiqi(1)
                   write(ifxvar,*) 'tchiqr(8),tchiqi(8)'
                   write(ifxvar,*)  tchiqr(8),tchiqi(8)
                end if
c
c  add this to the other paths
                do 850 i = 1, nqdata
                   thiq(i, id) = thiq(i, id) + tchiqi(i)
                   if (final) thiqr(i, id) = thiqr(i, id) + tchiqr(i)
 850            continue
             end if
 990         continue
 1000     continue
c   take fft of theory chi (exactly as for data chi)
          if (iprint.ge.2) then
             write(ifxvar,*) ' call fitfft: id = ', id
             write(ifxvar,*) ' xolow , xohigh ', xolow , xohigh
             write(ifxvar,*) ' maxpts, mftfit ', maxpts, mftfit
             write(ifxvar,*) ' qgrid =  ', qgrid
          endif
          call fitfft(thiq(1,id), maxpts, mftfit, wfftc, qgrid,
     $         qwindo(1,id), qweigh(id), rwindo(1,id), rweigh(id),
     $         ifft(id), xolow,xohigh, nfit1, thifit)
c
          if (nfit1.ne.nfit) then
             if (iprint.ge.1) then
                write(ifxvar,*) '*********************'
                write(ifxvar,*) 'fitfun error after fitfft: id = ',id
                write(ifxvar,*) 'nfit,  nfit1 = ', nfit, nfit1
                write(ifxvar,*) 'these should be equal !!'
                write(ifxvar,*) '*********************'
             end if
cc             print*, 'nfit,  nfit1 = ', nfit, nfit1
             call finmsg(3590,' ',' ',nfit1)
          endif
c  evaluate the contribution to fvec for this data set.  weight scales
c  chi-square properly to the number of independent points. this is
c  important for error analysis (if chi-square is to increase by one,
c  it  must be scaled correctly.), but only in the final pass, when
c  chi-square and r-factors will be calculated.
          if (final.and.iprint.ge.2) then
             write(ifxvar,*) ' in fitfun (final): id = ', id
             write(ifxvar,*) ' xolow , xohigh ', xolow , xohigh
             write(ifxvar,*) ' maxpts, mftfit ', maxpts, mftfit
             write(ifxvar,*) ' nfit, nfit1,jfit = ', nfit, nfit1,jfit
             write(ifxvar,*) ' rfactr(id)  = ', rfactr(id)
             if (iprint.ge.5)
     $            write(ifxvar,*) ' i, chifit,thifit,fvec: '
          end if
          do 2400 i =  1, nfit
             fvec(jfit+i) = (thifit(i) - chifit(i,id))/weight(id)
             if (final.and.iprint.ge.5) write(ifxvar,*) jfit+i,
     $            chifit(i,id), thifit(i),  fvec(jfit+i)
 2400     continue
          jfit  = nfit + jfit
          if (final.and.datain(id))
     $         rfactr(id) = rfact(chifit(1,id), thifit, nfit)
 3000  continue
       return
c  end subroutine fitfun
       end
