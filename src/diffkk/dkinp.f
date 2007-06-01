       subroutine dkinp
c read inputs for diffkk program from 'diffkk.inp'
c
       include "dkcom.f"
       integer  i, mfil, maxwrd,nwords, iwrds
       parameter(maxwrd = 30, mfil = 10)
       character*70   words(maxwrd), wrdsor(maxwrd), keywrd
       character*70   prompt*20,key*3,stat*10, string,str,comfil(mfil)
       integer   istrln,icom(mfil), nfil, iinp, ier, iex, ilen, nline
       external   istrln
c     initialization
c initialize  stuff in common
       active = .false.
       isfeff = .true.
       f2tof1 = .true.
       inpfil = 'diffkk.inp'
       xmufil = 'xmu.dat'
       outfil = 'dk.out'
       iatz   = -1
       iprint = 0
       imucol = 2
       iencol = 1
       egrid  = 1
       elow   = 200
       ehigh  = 500
       ewidth = -1.0
c note: pre1/pre2 will be set in f2fit according 
c       to whether data is from feff or not
       epad   = 5
       npad   = 20
       ntitle = 0
       label =' e         f1         f2         f1_cl         f2_cl'
       do 10 i = 1, mtitle
          title(i) = ' '
 10    continue 
c initialize  local stuff
       prompt = 'f'
       stat   = 'old'
       nline  = 0 
       nfil   = 0
       ier    = 0
       iex    = 0
       iinp   = 1
       do 20 i = 1, mfil
          icom(i)   = 0
 20    continue

       call get_inpfile('diffkk.inp',inpfil, ier)
       call openfl(iinp,  inpfil, stat, iex, ier)
       if (iex.lt.0)  then 
          ilen = istrln( inpfil)
          call messag( ' '// inpfil(:ilen)// ' not found')
          stop
          return
       endif
       if ((iex.lt.0).or.(ier.ne.0)) go to 1990
c
c  read in next line
 100   continue
       keywrd = ' '
       key    = ' '
       call getcom(prompt, iinp, string, comfil, icom, mfil, nfil)
       nline  = nline + 1
c      
       call fixstr(string,str,ilen,words,wrdsor,maxwrd,nwords)
       if ((ilen.le.2).or.(str.eq.'getcom_eof')) go to 100
       if ((words(1).eq.'end').or.(str.eq.'getcom_end')) go to 1000
       if (str.eq.'getcom_error') go to 1990
c      parse current set of keywords from line
 150   continue
       keywrd  = words(1)
       key     = keywrd(1:3)
       iwrds   = 2
c      inpfil,outfil,imucol,egrid,iatz
       if (keywrd.eq.'title') then 
          ntitle  = ntitle + 1
          call strclp(string,wrdsor(1),wrdsor(2),title(ntitle))
          iwrds = maxwrd + 1
       else if (key.eq.'out') then
          outfil = wrdsor(2)
       else if (key.eq.'xmu') then
          xmufil = wrdsor(2)
       else if (keywrd.eq.'isfeff') then
          call str2lg(words(2),isfeff,ier)
       else if (keywrd.eq.'f2_to_f1') then
          call str2lg(words(2),f2tof1,ier)
       else if (keywrd.eq.'f1_to_f2') then
          call str2lg(words(2),f2tof1,ier)
          f2tof1 = .not.f2tof1
       else if (keywrd.eq.'iz') then
          call str2in(words(2),iatz,ier)
       else if (keywrd.eq.'mucol') then
          call str2in(words(2),imucol,ier)
       else if (keywrd.eq.'encol') then
          call str2in(words(2),iencol,ier)
       else if (keywrd.eq.'egrid') then
          call str2dp(words(2),egrid,ier)
       else if (keywrd.eq.'e0') then
          call str2dp(words(2),e0,ier)
       else if (keywrd.eq.'ewidth') then
          call str2dp(words(2),ewidth,ier)

       else if (keywrd.eq.'elow') then
          call str2dp(words(2),elow,ier)
       else if (keywrd.eq.'ehigh') then
          call str2dp(words(2),ehigh,ier)
       else if (keywrd.eq.'epad') then
          call str2dp(words(2),epad,ier)
       else if (keywrd.eq.'npad') then
          call str2in(words(2),npad,ier)
       else if (key.eq.'iz') then
          call str2at(words(2),iatz)
       else if (key.eq.'iprint') then
          call str2at(words(2),iprint)
       end if
       if (nwords.gt.iwrds) then
          do 450 i = 1, nwords
             words(i)  = words(i+iwrds)
             wrdsor(i) = wrdsor(i+iwrds)
 450      continue
          nwords = nwords - iwrds
          go to 150
       end if
       go to 100
       
 1000  continue 
       return
 1990  continue 
       call messag(' weird, fatal error trying to open file.')
       stop
       end
