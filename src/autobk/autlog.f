       subroutine autlog(iofl)
c
c   a bunch of write statements to the log file (unit=iofl)
c   this should fully account what went on in the background removal.
c   please feel free to alter it in any way.
c
        include 'autobk.h'
       integer   iofl, ilen, ix, icom, istrln
       double precision  e0
       external  istrln
c
c if first time through, open log file
c begin writing results
       write(iofl,425) 
       icom = max(1, istrln(commnt))
       if (icom.ne.0) then
          write(iofl,400) ' '//commnt(:icom)
       write(iofl,426) 
       end if
       ilen = max(1, istrln(xmuf))
       write(iofl,410) ' input xmu data file name and skey: ',
     $                   xmuf(:ilen + 2),skeyxm
       write(iofl,405) '    first document line: ',
     $                   xmudoc(1)(:50)
       if (theory) then
         write(iofl,426)
         ilen = max(1, istrln(theorf))
         write(iofl,410) ' input theory chi file name and skey: ',
     $                   theorf(:ilen + 2),skeyth
         write(iofl,405) '    first document line: ',
     $                   thedoc(1)(:50)
       end if
       write(iofl,426)
       ix = min(55, max(1, istrln(chif)) )
       write(iofl,405) ' output chi file : ',chif(1:ix)
       write(iofl,426)
       write(iofl,400) ' --------fitting parameters---------'
       e0 = ee + e0shft
       if (theory.and.eevary) then
          write(iofl,460) ' initial value of e0        =  ',ee
          write(iofl,460) ' final value of e0          =  ',e0
       else
          write(iofl,460) '  e0 fixed at               =  ',ee
       end if
       write(iofl,470)    ' pre-edge range             =  ',
     $                                               predg1,predg2
       write(iofl,490)    ' pre-edge line              = ', slopre , 
     $                    ' * Energy + ', bpre
       write(iofl,460)    ' edge step                  =  ',step
       write(iofl,493)    ' post-edge curve for edge_step = ', 
     $      cnorm(1),  ' + ', cnorm(2), ' * Energy '
       write(iofl,494)    '                    + ', cnorm(2) ,
     $      '  * Energy^2'

       if (funnrm) then 
          write(iofl,405) ' note: chi(k) was normalized by the',
     $                    ' background function : '
          write(iofl,400) '       chi(k) = ( xmu(e) / bkg(e) ) - 1'
       end if 
       write(iofl,470)    ' energy range               =  ',
     $                                                    emin,emax
       qmax = qgrid*int( sqrt((emax - e0) * etok) / qgrid )
       if (emin.gt.e0) then
          qmin = qgrid*int( sqrt((emin - e0) * etok) / qgrid )
       else
          qmin = zero 
       end if
       write(iofl,470)    ' k range                    =  ',qmin,qmax
       write(iofl,460)    ' k weight                   =  ',qweigh
       write(iofl,400)    '   fourier transform window: '
       if (iwindo.eq.1) then
          write(iofl,460) ' hanning fraction           =  ',windo1
       elseif (iwindo.eq.2) then
          write(iofl,460) ' gaussian: dk               =  ',windo1
       elseif (iwindo.eq.3) then
          write(iofl,460) ' lorentzian: dk             =  ',windo1
       elseif (iwindo.eq.4) then
          write(iofl,470) ' parzen: dk1, dk2           =  ',
     $                                       windo1,windo2
       elseif (iwindo.eq.5) then
          write(iofl,470) ' welch: dk1, dk2            =  ',
     $                                       windo1,windo2
       else
          write(iofl,470) ' sills: dk1, dk2            =  ',
     $                                       windo1,windo2
       endif
       write(iofl,480)    ' # of knots in spline       =  ',nsplin
       write(iofl,470)    ' background r range         =  ',zero,rbkg
       if (theory) then
          write(iofl,460) ' the theory was scaled by   =  ',theamp
          write(iofl,470) ' 1st shell r range          =  ',rbkg,r1st
       end if
       write(iofl,425)

 400   format(2x,a)
 405   format(2x,2a)
 410   format(2x,3a)
 422   format(9x,'(1/2) + Amp * atan( (Energy - Emid) / Ewid )')
 425   format(3x,70('-'))
 426   format(3x,35('-'))
 460   format(2x,a,f15.6)
 470   format(2x,a,f15.6,1x,f15.6)
 480   format(2x,a,i4)
 490   format(2x,a,e13.6,a,e13.6)
 493   format(2x,a,e13.6,a,e13.6,a)
 494   format(2x,a,e13.6,a)

       return
c     end subroutine autlog
       end
