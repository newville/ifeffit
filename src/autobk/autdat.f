       subroutine autdat
c
c  this routine reads all input data files for the progam autobk.
c  the xmu data file is opened, the pre-edge is removed, and the 
c  standard chi data (if used) is read. the routine inpdat is 
c  used for all data files, allowing either uwexafs binary or 
c  ascii column data to be used.
c
c   copyright 1992  university of washington :          matt newville
c-----------------------------------------------------------------------
       include 'autobk.h'
c local variables
       character    ftype*5, frmthe*10
       double precision qtemp (maxpts), chitmp(maxpts), small 
       double precision q, rgrid, rsmall, rtemp
       double precision dummy1(maxpts), dummy2(maxpts)
       double precision dummy3(maxpts), xmuraw(maxpts)
       parameter (small  = 1.d-10)
       integer  ndocln, i, nxx, ipos, nemin, nemax, nofx
c--------------------------------------------------
       if (xmuf.eq. ' ') then 
          call echo('   autobk error: no input xmu data'//
     $                ' file name given.')
          call fstop(' no xmu data')
       end if
c 
c  get xmu data, do pre-edge and normalization, and store the
c  modified xmu values in xmudat.
       ndocln = maxdoc
       nxmu   = maxpts
       ftype  = 'xmu'
       irecl  = 512
       call inpdat( ftype, frminp,   xmuf, vaxflg, skeyxm, 
     $      nkeyxm, irecl, ndocln, xmudoc,   nxmu, energy,
     $      xmuraw, dummy1, dummy2, dummy3)

cc       print*, ' irecl = ', irecl, nxmu, energy(1)
c
c simple kludge to allow columnd 3,4,5 to be used as xmuraw:
       if ((imucol.ge.3).and.(imucol.le.5)) then
          if (imucol.eq.3) then
             do  22 i = 1, nxmu
                xmuraw(i) = dummy1(i)
 22          continue 
          elseif (imucol.eq.4) then
             do  23 i = 1, nxmu
                xmuraw(i) = dummy2(i)
 23          continue 
          elseif (imucol.eq.5) then
             do  24 i = 1, nxmu
                xmuraw(i) = dummy3(i)
 24          continue 
          endif
       endif
c
c remove pre-edge, get edge step
       nnorm = 2
       call preedg(eefind, stfind, nxmu, energy, xmuraw, ee,
     $      predg1, predg2, enor1, enor2, nnorm, 
     $      step, slopre, bpre,cnorm)
       step = max(step, small)
       if (funnrm) step = one
c set background equal to xmu 
       do 50 i = 1, nxmu
          xmudat(i) = xmuraw(i) - slopre * energy(i) - bpre
          spline(i) = xmudat(i)
 50    continue
c
c  get theory from a 'chi' file
c  if theory is not given thiq is filled with 0.0
       ntheor = 1
       qtemp(1) = zero
       if (theory) then
          ndocln = maxdoc
          ntheor = maxpts
          ftype  =  'chi'
c           frmthe = frminp
          frmthe = ' '
          call inpdat(ftype,  frmthe, theorf, vaxflg, skeyth, 
     $         nkeyth, irecl, ndocln, thedoc, ntheor,  qtemp,
     $         chitmp, dummy1, dummy2, dummy3)
       end if
       nxx  = min(maxpts, 10 + int (qtemp(ntheor) / qgrid))
       ipos = 1
       do 200 i = 1, nxx
          q = i*qgrid
          if ( (q.lt.qtemp(1)).or.(q.ge.qtemp(ntheor)) ) then
              thiq(i) = zero
          else
              call lintrp ( qtemp, chitmp, ntheor, q, ipos, thiq(i) )
          end if
 200   continue
c-----------------------------------------------------------------------
c  move emin and emax to values on the energy grid
       emin = emin + ee
       emax = emax + ee
       if (emax.le.emin) emax = energy(nxmu)
       nemin = nofx(emin,energy,nxmu)
       nemax = nofx(emax,energy,nxmu)
       emin  = energy(nemin)
       emax  = energy(nemax)
c-----------------------------------------------------------------------
c  put r values on rgrid, get npts numbers
       if (r1st.le.rbkg) r1st = rbkg + 2.0
       rgrid  = pi / (qgrid * mftfit)
       rsmall = rgrid / 100.0
       rbkg   = rgrid * int( (rbkg + rsmall) / rgrid ) 
       r1st   = rgrid * int( (r1st + rsmall) / rgrid ) 
       if (rbkg.gt.r1st) then
            rtemp = rbkg
            rbkg  = r1st
            r1st  = rtemp
       end if
       nrbkg  = 2 * int((rbkg + rsmall)/rgrid) + 2
       nrpts  = 2 * int((r1st + rsmall)/rgrid) + 2
       nr1st  = nrpts - nrbkg

       if (nxmu.le.4) then
          call echo('   autobk error: data file empty, or not'//
     $         ' enough data points')
          call echo('      the data file may need # signs for'//
     $         ' comment lines.')
          call fstop(' bad data file')
       end if

c done
       return
c end subroutine autdat
       end
