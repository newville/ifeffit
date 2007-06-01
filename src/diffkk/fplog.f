       subroutine fplog
c
c write log of diffkk.  note that diffkk.log *can* be used as diffkk.inp
       include "dkcom.f"
       integer   istrln, ilog, ier, iex, ilen, i
       character*70    stat*10, logfil, str, atsym*2
       external   istrln, atsym
       data stat   /'unknown'/
c----------------------------------------------------------------------
c  determine log file name (usually trivial)
c  but if not, find the *last* "."
       logfil = 'deltaf.log'
c  open log file
       ilog   = 0
       ier    = 0
       iex    = 0
       call openfl(ilog, logfil, stat, iex, ier)
       if (ier.lt.0) go to 5000
c
c write version number
       write(str,10) versn
 10   format(' --  deltaf version ',a, '--')
       ilen = istrln(str)
       write(ilog,900) str(1:ilen)
       write(ilog,900)   ' program inputs or default values used:' 
       if (ntitle.le.0) then 
          write (ilog, 910)  'title',
     $         'deltaf: no title lines specified'
       else
          do 100 i = 1, ntitle
             ilen = max(1, istrln(title(i)))
             write (ilog, 910)  'title', title(i)(1:ilen)
 100      continue 
       end if
       ilen = max(15, istrln(outfil))
       write (ilog, 930)  'iz', iatz, 'atomic number of core atom'
       write (ilog, 920)  'out', outfil(1:ilen), 'output file name'
c
       write (ilog, 950)  'elow', elow,
     $      'low-energy of calculation range'
       write (ilog, 950)  'ehigh', ehigh,
     $      'high-energy of calculation range'
       write (ilog, 950)  'egrid', egrid,'energy grid for calculation'
       write (ilog, 950)  'ewidth', ewidth,'for broadening CL data'
c
       write(ilog,905) ' end % '//
     $      'all remaining lines will be ignored on input'
       write(ilog,905) ' -- end deltaf log file --'
c
       close(ilog)
       return
 900   format(1x,'# ',a)
 905   format(1x, a)
 910   format(1x, a8, ' = ', a)
 920   format(1x, a8, ' = ', a    ,  1x,'% ',a)
 930   format(1x, a8, ' = ', i4   , 12x,'% ',a)
 950   format(1x, a8, ' = ', f11.4,  5x,'% ',a)
 955   format(6x, a10, ' = ', g15.6)
 5000  continue 
       call messag(' weird, fatal error trying to open input file')
       stop
       end
