      program feff
c
c  Experimental Feff6: work in progress  -- Matt Newville
c  This is meant to make an EXAFS only version of FEFF6 that
c  somewhat simplified in its options, and more modular.
c 
c  Modified by Matt Newville from original code by John Rehr
c  see LICENSE for copying details
c
       implicit none
       include 'vers.h'
       include 'dim.h'
       include 'const.h'

       integer  mtitle
       parameter (mtitle = 16)
       character*128  titles(mtitle),  fname*64

       integer ibeta, ipotn, ik0, ipot, ios, ne, ie
       integer nlegx, nncrit, isporb, ipotnn, i, nlegxx
       integer ipr2, ipr3, ipr4,  iorder, icsig
       integer mphase, mpath, mfeff, mchi, ms, ntitle
       double precision s02, tk, thetad, sig2g, critcw
       double precision  angle, cosb, vicorr, vrcorr


       integer necrit, nbeta
       parameter (necrit=9, nbeta=40)
       double precision fbetac(-nbeta:nbeta,0:npotx,necrit)
       double precision fbeta(-nbeta:nbeta,0:npotx,nex)

       double precision ckspc(necrit), cksp(nex)
       double precision critpw, pcritk, pcrith
       character*6  potlbl(0:npotx)

       character*512 inputfile, geomfile, potfile
       double precision  rmax, rmult, viexch, vrexch, rsexch
       double precision  vpolar(3), vellip(3)
       integer iexch, iedge

       integer istat, il, iox, istrln, ilen
       external istrln

   10 format (1x, a)
       vfeff = 'Feff6LX.01'

       call sca_init
       call echo_init
       call open_echofile('feff6.run')
       call fstop_init('feff6.err')
       call echo(vfeff)

       call get_inpfile('feff6.inp',inputfile,istat)

       istat = 0

       call ReadFeffInp(inputfile, geomfile, potfile, titles, mtitle,
     $      iedge, iexch, viexch, vrexch, rsexch,
     $      rmult, rmax, vpolar, vellip, istat)

c istat .ne. 0 means an error reading the input file (no file??)
       if (istat.ne.0) return
       
       do 20  i = 1, mtitle
          ilen = istrln(titles(i))
          if (ilen.ge.1) then
             call echo('    '//titles(i)(1:istrln(titles(i))))
          endif
 20    continue
       

       call echo( 'Calculating potentials and phases...')
       istat = 0

       call Potentials(geomfile, potfile, iedge,
     $      iexch, viexch, vrexch, rsexch, rmult, istat)


       print*, 'Feff6X Devel: Stopping after Potentials.....'

       stop


      if (ms.eq.1  .and.  mpath.eq.1)  then

         call echo('Preparing plane wave scattering amplitudes...')
         call prcrit (ne, nncrit, ik0, cksp, fbeta, ckspc, 
     1                fbetac, potlbl)

c        Dump out fbetac for central atom and first pot
         if (ipr2 .ge. 3 .and. ipr2.ne.5)  then
            do 260  ipot = 0, 1
               do 250  ie = 1, nncrit
                  write(fname,200)  ie, ipot
  200             format ('fbeta', i1, 'p', i1, '.dat')
                  open (unit=1, file=fname)
                  write(1,210)  ipot, ie, ckspc(ie)
  210             format ('# ipot, ie, ckspc(ie) ', 2i5, 1pe20.6, /
     1                    '#  angle(degrees), fbeta/|p|,  fbeta')
                  do 240  ibeta = -nbeta, nbeta
                     cosb = .025 * ibeta
                     if (cosb .gt.  1)  cosb =  1
                     if (cosb .lt. -1)  cosb = -1
                     angle = acos (cosb)
                     write(1,230)  angle*raddeg, 
     1                  fbetac(ibeta,ipot,ie)/ckspc(ie),
     2                  fbetac(ibeta,ipot,ie)
  230                format (f10.4, 1p, 2e15.6)
  240             continue
                  close (unit=1)
  250          continue
  260       continue
         endif

         call echo('Searching for paths...')
         call paths (ckspc, fbetac, pcritk, pcrith, nncrit,
     1               rmax, nlegxx, ipotnn)

         call echo('Eliminating path degeneracies...')
         call pathsd (ckspc, fbetac, ne, ik0, cksp, fbeta,
     1                critpw, ipotnn, ipr2, 
     1                pcritk, pcrith, nncrit, potlbl)

c         if (ipr2 .lt. 2)  then
c           open (unit=1, file='geom.dat', status='old')
c           call chopen (ios, 'geom.dat', 'feff')
c           close (unit=1, status='delete')
c         endif
      endif

      if (mfeff .eq. 1)  then
         call echo('Calculating EXAFS parameters...')
         call genfmt (ipr3, critcw, sig2g, iorder)
      endif

c      if (mchi .eq. 1)  then
c         call echo('Calculating chi...')
c         call ff2chi (ipr4, critcw, s02, tk, thetad, icsig,
c     1        vrcorr, vicorr)
c      endif

       call echo('Feff done.  Have a nice day.')
       end
