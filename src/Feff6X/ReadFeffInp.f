      subroutine ReadFeffInp(inputfile, geomfile, potfile,
     $     titles, mtitle,
     $     iedge, iexch, viexch, vrexch, rsexch,
     $     rmax, vpolar, vellip, istat)

c 
c read new-style feff.inp
c
c args: 
c    inputfile   name of input parameter file   (feff6.inp) [inp]
c    titles      array of title lines                       [out]
c    mtitle      max number of titles                       [inp]
c    geomfile    name of xyz coordinates file   (atoms.xyz) [out]
c    potfile     name of potentials/phase file  (atoms.xyz) [out]
c    iedge       index for absorption edge      (0)         [out]
c    iexch       index of exchange mode         (0)         [out]
c    viexch      imag potential correction (eV) (0.0)       [out]
c    vrexch      real potential correction (eV) (0.0)       [out]
c    rsexch      DH/HL transition threshold (eV)(0.0)       [out]
c    rmax        max dist (AA) for cluster      (10.0)      [out]
c    vpolar      polarization vector            (0,0,0)     [out]
c    vellip      ellipticity vector             (0,0,0)     [out]
c    istat       output status (0 for success, >1 failure)  [out]
c
      implicit none
      integer  iedge, iexch, mtitle, istat
      double precision rmax, viexch, vrexch, rsexch
      double precision vpolar(3), vellip(3)

      character*128  titles(mtitle)
      character*(*)  inputfile, geomfile, potfile

      character*1024  line, tmpstr
      integer istrln, ilen, jlen, iflen, iret, i, ierr, ios
      integer mwords, nwords, ititle, iread
      logical iscomm, debug
      parameter (mwords = 16)
      character*32 words(mwords), key
     
      external istrln, iscomm, iread


      debug = istat.eq.1

      iflen = istrln(inputfile)

      if (debug) print*, 'This is ReadFeffInp ', inputfile(1:iflen)

      istat  = 1

      open (unit=1, file=inputfile, status='old', iostat=ios)
      if (ios .gt. 0)  then
         istat = ios
         write(tmpstr,10) inputfile(1:iflen)
 10      format ("Feff6 cannot open Parameter file '",a, "'")
         call echo(tmpstr)
         return
      endif 

c  set default params
      ititle = 0
      iedge  = 0
      iexch  = 0
      rmax   = 10.d0
      viexch = 0.d0
      vrexch = 0.d0
      rsexch = 0.d0
      geomfile = 'atoms.xyz'
      potfile  = 'potentials.bin'

      do 20 i = 1, 3
         vpolar(i) = 0.d0
         vellip(i) = 0.d0
 20   continue 
      do 30 i = 1, mtitle
         titles(i) = ''
 30   continue 

      
c  reading loop for lines in inputfile
 100  continue 
      iret = iread(1, line)

      if (iret.eq. 0) goto  100     ! blank line, get next
      if (iret.le.-1) goto  300     ! end of input, finish loop
      call triml(line)
      if (iscomm(line))  goto 100
      nwords = mwords
      do 105 i = 1, 5 
         words(i) = '0.0'
 105  continue 
      call bwords(line, nwords, words)

c get keyword (first word, lowercase) and  the rest of the line
      key = words(1)
      call lower(key)
      
      ilen = istrln(key)
      line  = line(ilen+1:iret)
      call triml(line)
      jlen =  istrln(line)
      
cc      # if (debug) print*, ' Input Line: ', key, ' :', line(1:jlen)

      if ((key.eq.'title').and.(ititle.lt.mtitle)) then
         ititle = ititle + 1
         titles(ititle) = line
         if (debug)  print*, '# Title   : ', line(1:jlen)
      elseif (key.eq.'quit') then
         goto 300
      elseif (key.eq.'hole') then
         call ReadInp_edge(words(2),iedge)
         if (debug)  print*, '# Hole   : ', iedge

      elseif (key.eq.'exchange') then 
         call ReadInp_exchange(words(2),iexch)
         call str2dp(words(3),vrexch, ierr)
         call str2dp(words(4),viexch, ierr)
         call str2dp(words(5),rsexch, ierr)
         if (debug) print*, '# Exch   : ', iexch,vrexch,viexch,rsexch
      elseif (key.eq.'polarization') then 
         call str2dp(words(2),vpolar(1), ierr)
         call str2dp(words(3),vpolar(2), ierr)
         call str2dp(words(4),vpolar(3), ierr)
         if (debug)  print*, '# Polar  : ', vpolar
      elseif (key.eq.'ellipticity') then 
         call str2dp(words(2),vellip(1), ierr)
         call str2dp(words(3),vellip(2), ierr)
         call str2dp(words(4),vellip(3), ierr)
         if (debug) print*, '# Ellip  : ', vellip
      elseif (key.eq.'rmax') then 
         call str2dp(words(2),rmax, ierr)
         if (debug)   print*, '# Rmax  : ', rmax
      elseif (key.eq.'geometry') then 
         geomfile = line
         if (debug) print*, '# Geomfile: ',geomfile(1:istrln(geomfile))
      elseif (key.eq.'potentials') then 
         potfile = line
         if (debug) print*, '# Potfile: ',potfile(1:istrln(potfile))
      else
         print*, '# Unknown input in Feff6 input ', inputfile(1:iflen)
         print*, key, line
      endif
      goto 100

 300  continue 
      close(unit=1)
      istat  = 0
ccc
      return 
      end
      subroutine ReadInp_exchange(word,iexch)
c
c return exchange model index
c
c   allowed inputs    output    meaning
c      0, HL       ->   0     Hedin-Lundqvist   + const real&imag
c      1, DH       ->   1     Dirac-Hara        + const real&imag
c      2, GS       ->   2     Ground State      + const real&imag
c      3, DHHLI    ->   3     DH + HL Imag part + const real&imag
c      4, DHRS     ->   4     DH below rs0, HL above rs0 + const real&imag
c
c  the input is insensitive to case
c
      character*(*) word
      character*16  key
      integer  iexch
      key = word(1:16)
      call lower(key)
      
      iexch = 0

      if ((key.eq.'dh').or.(key.eq.'1')) then
         iexch = 1
      else if ((key.eq.'gs').or.(key.eq.'2')) then
         iexch = 2
      else if ((key.eq.'dhhli').or.(key.eq.'3')) then
         iexch = 3
      else if ((key.eq.'dhrs').or.(key.eq.'4')) then
         iexch = 4
      endif 
      return 
      end

      subroutine ReadInp_edge(word,iedge)
c
c
c return edge index
c
c   allowed inputs    output   
c      1, K         ->   1     
c      2, L1        ->   2     
c      3, L2        ->   3     
c      4, L3        ->   4     
c
c  the input is insensitive to case

c       
      character*(*) word
      character*8  key
      integer iedge
      key = word
      call lower(key)

      iedge = 1

      if ((key.eq.'k').or.(key.eq.'1')) then
         iedge = 1
      else if ((key.eq.'l1').or.(key.eq.'li').or.(key.eq.'2')) then
         iedge = 2
      else if ((key.eq.'l2').or.(key.eq.'lii').or.(key.eq.'3')) then
         iedge = 3
      else if ((key.eq.'l3').or.(key.eq.'liii').or.(key.eq.'4')) then
         iedge = 4
      endif 

      return 
      end


