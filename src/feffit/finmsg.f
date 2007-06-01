       subroutine finmsg(ikey, str1, str2, i1)
c  error messages from feffit
       integer        ikey, i1, ilen
       character*(*)  str1, str2,lblnk*12,messg*80,s1tmp*90,s2tmp*90
       character*35   inperr, fterr, callme, warn, daterr,fferr,chkerr
       parameter (inperr = '>>error reading input file')
       parameter (chkerr = '>>error in math expressions')
       parameter (daterr = '>>error reading data file')
       parameter (fferr  = '>>error reading feff file')
       parameter (fterr  = '>>feffit error')
       parameter (warn   = '>>feffit warning')
       parameter (callme = 'program error: contact matt')
       parameter (lblnk  = '      ')

       s1tmp = ' '
       s2tmp = ' '
       if (str1.ne.' ') s1tmp = str1
       ilen1 = min(65,max(1,istrln(s1tmp)))
       if (str2.ne.' ') s2tmp = str2
       ilen2 = min(65,max(1,istrln(s2tmp)))

 11    format(a,i4)
 12    format(2a,i4)

       if (i1.lt.0) then
          call echo(lblnk//warn)
       else
          messg = fterr
          if ((ikey.ge.2000).and.(ikey.lt.3000)) messg = inperr
          if ((ikey.ge.3000).and.(ikey.lt.3200)) messg = daterr
          if ((ikey.ge.3200).and.(ikey.lt.3500)) messg = chkerr
          if ((ikey.ge.7000).and.(ikey.lt.8000)) messg = fferr
          call echo(lblnk//messg(1:65))
       end if
       if (ikey.eq.0) then
          call echo(lblnk//'feffit died')
       elseif (ikey.eq.1001) then
          call echo(lblnk//'could not find file: '//s1tmp(:ilen1))
       elseif (ikey.eq.1002) then
          call echo(lblnk//'error opening file: '//s1tmp(:ilen1))
       elseif (ikey.eq.1003) then
          call echo(lblnk//'error reading file: '//s1tmp(:ilen1))
c from fitinp (all fatal)
       elseif (ikey.eq.2100) then
          write(messg,11) 'too many named values! current limit is',i1
          ilen = istrln(messg)
          call echo(lblnk//messg(1:ilen))
       elseif (ikey.eq.2105) then
          write(messg, 12) 'too many "local" ',
     $      'user-defined values! current limit is ', i1
          ilen = istrln(messg)
          call echo(lblnk//messg(1:ilen))
       elseif (ikey.eq.2110) then
          call echo('this named value '//
     $      'was assigned as both "local" and "global":')
          call echo(lblnk//s2tmp(1:ilen2))
       elseif (ikey.eq.2120) then
          call echo(lblnk//'error encoding the math expression :')
          call echo(lblnk//'  --> '//s2tmp(:ilen2))
       elseif (ikey.eq.2130) then
          write(messg,12) 'too many variables! ',
     $         'current limit is ', i1
          ilen = istrln(messg)
          call echo( '       '//messg(1:ilen))
       elseif (ikey.eq.2140) then
          write(messg,12) 'too many paths for a ',
     $         'data file! current limit is ',i1
          ilen = istrln(messg)
          call echo(lblnk//messg(1:ilen))
          messg = 'for data file: '//s2tmp(:ilen2)
          ilen = istrln(messg)
          call echo(lblnk//messg(1:ilen))
       elseif (ikey.eq.2150) then
          write(messg,11) 'too many paths used. current limit is',i1
          ilen = istrln(messg)
          call echo(lblnk//messg(1:ilen))
       elseif (ikey.eq.2170) then
          write(messg,12) 'too many feffnnnn.dat ',
     $          'files used! current limit is ',i1
          ilen = istrln(messg)
          call echo(lblnk//messg(1:ilen))
          messg = 'file requested was '//s2tmp
          ilen = istrln(messg)
          call echo(lblnk//messg(1:ilen))
       elseif(ikey.eq.2200) then
          call echo(lblnk//'a Path Index larger than '//
     $      '999 or less than 0 was found! ')
       elseif (ikey.eq.2220) then
          write(messg,11) 'too many data sets. current limit is',i1
          ilen = istrln(messg)
          call echo(lblnk//messg(1:ilen))
       elseif (ikey.eq.2300) then
          call echo(lblnk//'too many unknonwn keywords!')
          call echo(lblnk//'something wrong with the input file?')
       elseif (ikey.eq.2330) then
          messg = 'unknown keyword : "'//s2tmp(:ilen2)//
     $         '" at this line '
          ilen  = istrln(messg)
          call echo(lblnk//messg(1:ilen))
          call echo(lblnk//'line:  '//s1tmp(:ilen1))
          call echo(lblnk//'the rest of this line will be ignored')
c from fitdat
       elseif (ikey.eq.3010) then
          call echo(lblnk//'data appears to not be chi(k) data')
          call echo(lblnk//'check data file and consult manual')
       elseif (ikey.eq.3020) then
          call echo(lblnk//'too many variables while adding')
          call echo(lblnk//'adding background spline to fit')
          write(messg,11) 'current limit is',i1
          ilen = istrln(messg)
          call echo(lblnk//messg(1:ilen))
       elseif (ikey.eq.3040) then
          call echo(lblnk//'too many named values while adding')
          call echo(lblnk//'adding background spline to fit')
          write(messg,11) 'current limit is',i1
          ilen = istrln(messg)
          call echo(lblnk//messg(1:ilen))
c from fitchk
       elseif (ikey.eq.3200) then
          call echo(lblnk//'inconsistent number of variables')
          call echo(lblnk//'some variable is probably both'//
     $         ' "guessed" and "set".')
       elseif (ikey.eq.3220) then
          call echo(lblnk//'this variable name was used but '//
     $             'was not defined:' )
          call echo(lblnk//'  --> '//s1tmp(:ilen1))
       elseif (ikey.eq.3240) then
          call echo(lblnk//'the following value was defined but is')
          call echo(lblnk//'not used in any math expressions:')
          call echo(lblnk//'  --> '//s1tmp(:ilen1))
          call echo(lblnk//'this may cause problems with the fit.')
       elseif (ikey.eq.3300) then
          call echo(lblnk//'bad initial value for an XAFS parameter!')
          call echo(lblnk//'   for '//s1tmp(1:ilen1))
          call echo(lblnk//'       '//s2tmp(1:ilen2))
c from fitnls
       elseif (ikey.eq.3510) then
          call echo(lblnk//'more variables than measurements')
          call echo(lblnk//s1tmp(:ilen1))
       elseif (ikey.eq.3530) then
          call echo(lblnk//'fit gave an impossible error message.')
          call echo(lblnk//callme)
c from fitfun
       elseif (ikey.eq.3590) then
          call echo('routine fitfft failed internal test.')
          call echo(lblnk//callme)
c from fefinp
       elseif (ikey.eq.7010) then
          call echo(lblnk//s1tmp(:ilen1))
          call echo(lblnk//'bad data in feffnnnn.dat file')
       elseif (ikey.eq.7020) then
          write(messg,11) 'too many legs in path. current limit is',i1
          ilen = istrln(messg)
          call echo(lblnk//messg(1:ilen))
          call echo(lblnk//s1tmp(:ilen1))
       elseif (ikey.eq.7050) then
          call echo(lblnk//'not enough room '//
     $         'to read all the data from this feff file')
          write(messg,'(a,i3,a)') 'results above k = ',
     $         -i1, ' will not be reliable'
          ilen = istrln(messg)
          call echo(lblnk//messg(1:ilen))
       elseif (ikey.eq.7510) then
          call echo(lblnk//s1tmp(:ilen1))
          call echo(lblnk//'bad data in feff.bin file')
c ??
       else
          write(messg,11) 'unknown error ',ikey
          ilen      = max(1, istrln(messg))
          call echo(lblnk//messg(1:ilen))
       endif
c for fitinp messages, write out last line from feffit.inp
       if ((ikey.ge.2100).and.(ikey.le.3000).and.(i1.ge.0)) then
          call echo(lblnk//'last line read successfully:')
          call echo(lblnk//s1tmp(:ilen1))
       endif
       if ((ikey.le.9999).and.(i1.ge.0))
     $      call fstop('feffit error')
       return
       end
