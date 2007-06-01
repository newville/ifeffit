       subroutine dklog
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
       logfil = 'diffkk.log'
c  open log file
       ilog   = 0
       ier    = 0
       iex    = 0
       call openfl(ilog, logfil, stat, iex, ier)
       if (ier.lt.0) go to 5000
c
c write version number
       write(str,10) versn
 10   format(' --  diffkk version ',a, '--')
       ilen = istrln(str)
       write(ilog,900) str(1:ilen)
       write(ilog,900)   ' program inputs or default values used:' 
       if (ntitle.le.0) then 
          write (ilog, 910)  'title',
     $         'diffkk: no title lines specified'
       else
          do 100 i = 1, ntitle
             ilen = max(1, istrln(title(i)))
             write (ilog, 910)  'title', title(i)(1:ilen)
 100      continue 
       end if
       ilen = max(15, istrln(outfil))
       write (ilog, 920)  'out', outfil(1:ilen), 'output file name'
c
       ilen = max(15, istrln(xmufil))
       write (ilog, 920)  'xmu', xmufil(1:ilen),
     $      'file name for xmu data'
c
       if (isfeff) then
          write (ilog, 981)'isfeff','file is a feff xmu.dat file'
       else 
          write (ilog, 980)'isfeff','file is not a feff xmu.dat file'
       end if
       if (f2tof1) then
          write (ilog, 981)'f2_to_f1', 'transform f2-like data to f1'
       else 
          write (ilog, 980)'f2_to_f1', 'transform f1-like data to f2'
       end if
       write (ilog, 930) 'encol',iencol,'column to read energy from'
       write (ilog, 930) 'mucol',imucol,'column to read mu(E) from'
       write (ilog, 930)  'iz', iatz, 'atomic number of core atom'
       write (ilog, 950)  'e0', e0 , 'edge energy (used as reference)'
c
       write (ilog, 950)  'egrid', egrid,'energy grid for calculation'
       write (ilog, 950)  'ewidth', ewidth,'for broadening CL data'

       write (ilog, 950)  'elow', elow,
     $      'how far below data range to calculate'
       write (ilog, 950)  'ehigh', ehigh,
     $      'how far above data range to calculate'
       if (isfeff) then
          write(ilog,900) 'the following are for padding '//
     $         'data from feff''s xmu.dat below e0'
          write (ilog, 950) 'epad', epad, 'energy grid for pre-padding'
          write (ilog, 930) 'npad', npad,
     $         'number of points for pre-padding'
       end if
c
       write(ilog,905) ' end  % '//
     $      'all remaining lines will be ignored on input'
       write(ilog,905) ' -- diffkk program summary --'
       write(ilog,905) ' '
       write(ilog,905) '   f''''(E) was set to be'
       write(ilog,905) '        f''''(E)  = f''''_CL(E) '//
     $      ' for E > e0+ehigh and  E < e0-elow.'
       write(ilog,906) '   within the range E = [e0-elow, e0+ehigh],',
     $      ' f'''' was set to'
       write(ilog,905) '        f''''(E)  = f''''_CL(E) + a0 '//
     $      '+ a1 *  mu(E'') * ( E'' /  e0)'
       write(ilog,905) '                + a2 * (E'' - e0) '//
     $      '+ a3 * (E'' - e0)**2 '
       ilen = max(1,istrln(xmufil))
       write(ilog,907) '   where mu(E) was read from ',
     $      xmufil(1:ilen),','
       write(ilog,907) '   f''''_CL(E) was found for ',
     $      atsym(iatz), ', and E'' = E + e0_shift.'
       write(ilog,905) ' '
       write(ilog,905) '   the values of e0_shift, a0, a1, a2, '//
     $      'and a3 were determined to be:'
       write(ilog,955) 'e0_shift', xvarys(1)
       write(ilog,955) 'a0', xvarys(2)
       write(ilog,955) 'a1', xvarys(3)
       write(ilog,955) 'a2', xvarys(4)
       write(ilog,955) 'a3', xvarys(5)
       write(ilog,905) '   so as to best match f''''_CL(E)'//
     $  ' between E = [e0-elow, e0+ehigh]'
       write(ilog,905) ' '
       write(ilog,905) ' -- end diffkk log file --'
c
       close(ilog)
       return
 900   format(1x,'# ',a)
 905   format(1x, a)
 906   format(1x, 2a)
 907   format(1x, 3a)
 910   format(1x, a8, ' = ', a)
 920   format(1x, a8, ' = ', a    ,  1x,'% ',a)
 930   format(1x, a8, ' = ', i4   , 12x,'% ',a)
 950   format(1x, a8, ' = ', f11.4,  5x,'% ',a)
 955   format(6x, a10, ' = ', g15.6)
 980   format(1x, a8, ' = false',   11x,'% ',a)
 981   format(1x, a8, ' = true',    12x,'% ',a)
 5000  continue 
       call messag(' weird, fatal error trying to open input file')
       stop
       end
