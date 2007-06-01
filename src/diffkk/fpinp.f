       subroutine fpinp
c read inputs for deltaf program from 'deltaf.inp'
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
       inpfil = 'deltaf.inp'
       xmufil = ' '
       outfil = 'df.out'
       iatz   = 8
       iprint = 0
       imucol = 2
       iencol = 1
       egrid  = 1
       elow   = 3000
       ehigh  = 5000
       ewidth = 1.5
c note: pre1/pre2 will be set in f2fit according 
c       to whether data is from feff or not
       epad   = 5
       npad   = 20
       ntitle = 0
       label =' e         fp         fpp'
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
c   input file: if file is found, open it for reading,
       call openfl(iinp,  inpfil, stat, iex, ier)
       if (iex.lt.0)  then 
          ilen = istrln(inpfil)
          call messag( ' '// inpfil(:ilen)// ' was not found'//
     $         ' --  let''s try this interactively')
          active = .true.
       endif
c
       if (active) then
          call askint(' > atomic number',iatz)
          call messag('   Please give the low- and high-energy range')
          call messag('   for the calculatin   in eV:')
          call askdp(' > elow        ',elow)
          ehigh  = 1000 + elow
          call askdp(' > ehigh       ',ehigh)
          call askdp(' > energy grid ',egrid)
          call messag('   Please give a width for broadening the '//
     $         'Cromer-Libermann calculation:')
          call askdp(' > ewidth      ',ewidth)
       else
c  read in next line
          if ((iex.lt.0).or.(ier.ne.0)) go to 1990
 100      continue
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
 150      continue
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
          else if (keywrd.eq.'iz') then
             call str2in(words(2),iatz,ier)
          else if (keywrd.eq.'ewidth') then
             call str2dp(words(2),ewidth,ier)
          else if (keywrd.eq.'egrid') then
             call str2dp(words(2),egrid,ier)
          else if (keywrd.eq.'elow') then
             call str2dp(words(2),elow,ier)
          else if (keywrd.eq.'ehigh') then
             call str2dp(words(2),ehigh,ier)
          end if
          if (nwords.gt.iwrds) then
             do 450 i = 1, nwords
                words(i)  = words(i+iwrds)
                wrdsor(i) = wrdsor(i+iwrds)
 450         continue
             nwords = nwords - iwrds
             go to 150
          end if
          go to 100
       end if
 1000  continue 
       close (iinp)
c get energy range       
       npts  = min (mpts, max (1, int(( ehigh - elow) / egrid)))
       do 1200 i = 1, npts
          energy(i)  = elow + egrid * i
 1200   continue 
       return
 1990  continue 
       call messag(' weird, fatal error trying to open file.')
       stop
       end


