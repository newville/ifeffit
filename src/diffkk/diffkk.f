       program diffkk 
c 
c  generate f' and f'' near x-ray resonances for an atom, including 
c  fine-structure due to solid-state effects (ie, xafs/dafs). 
c
c algorithm summary:
c 1 the brennan-cowan implementation of the cromer-libermann (cl) 
c   calculation is used as a starting set of a causal f' and f''. 
c   modifications were made to the bc code, mostly to make it easier 
c   to use, more closely f77 conforming, and smaller.  this data is
c   convolved with a lorenztian, typically with a width of a few ev.
c
c 2 an externally supplied file containing the xafs mu(e) is used to 
c   improve f''.  special support for xmu.dat files from feff is 
c   provided, or the external file can contain *measured* mu(e) for
c   the system of interest.  a simple matching procedure is done to 
c   make the supplied mu(e) match the f'' from cl.  
c
c 3 a differential kramers-kronig transform is used to convert the 
c   changes in f'' (ie f''_supplied - f''_cl) into the changes in  f' 
c   (ie f' - f'_cl).   the kk transform is done using a maclaurin 
c   series method, as suggested in the literature. 
c
c  the result is a causal pair of f' and f'' that reflect the 
c  presence of the atoms neighboring the central atom.
c
c  --  Further notes on the algorithms used and instructions for  --
c  --  program use are given in the program documentation.        --
c
c  copyright 1997,...,2003  matt newville
c  
c  acknowledgements: julie cross, chuck bouldin, john rehr, and
c            bruce ravel contributed to the design of this code.  
c            the best ideas were theirs.  all mistakes are mine. 
c  
c  1.21 using ifeffit atomic data for core-width if ewidth=0 on input
c  
       include 'dkcom.f'
       include '../lib/ifeffit.inc'
       integer  i, ncol, mcol, ierr, ilen
       parameter (mcol = 6)
       double precision  df1(mpts), df2(mpts)
       double precision  o1(mpts), o2(mpts),  atz
       double precision  pre_s1, pre_s2, pre_o1, pre_o2, pre_e1, pre_e2
       character*128     str
       integer  istrln, guess_iz
       external istrln, guess_iz

       versn = '1.3'
       ncol  = mcol
       ndoc  = mdoc
       npts  = mpts
c print version number
       i = ifeffit(' ')
       write(str,'(3a)') ' --  diffkk version ', versn, '--'
       ilen = istrln(str)
       call messag(str(1:ilen))
c read diffk.inp 
       call dkinp
c read mu(e) data, convert to f''(e) on an even energy grid
       write(str,'(2a)')  ' Reading experimental data: ',
     $      xmufil
       ilen = istrln(str)
       call messag(str(1:ilen))

       write(str,'(3a)')  'read_data(file=',
     $      xmufil(1:istrln(xmufil)),',group=dat,type=raw)'
       i = ifeffit(str)
       i = iffgetstr('column_label',str)
       if (str(1:13).eq.'--undefined--') then
          call messag( '  diffkk: fatal error.')
          goto 999
       endif
c
       

       write(str,'(a,i1)')  'set dat.energy = dat.',iencol
       if (iencol .ge.10) then
          write(str,'(a,i2)')  'set dat.energy = dat.',iencol          
       endif
       i = ifeffit(str)
       write(str,'(a,i1)')  'set dat.expdat = dat.',imucol
       if (imucol .ge.10) then
          write(str,'(a,i2)')  'set dat.expdat = dat.',imucol          
       endif
       i    = ifeffit(str)
       i    = iffgetarr('dat.energy', energy)
       npts = iffgetarr('dat.expdat', expdat)

       if (iatz .le.2)  then
          iatz = guess_iz(energy, expdat, npts, e0)
       end if

       i = iffputsca('e0',  e0)
       i = iffputsca('egrid',  egrid)
       i = iffputsca('elow',  elow)
       i = iffputsca('ehigh',  ehigh)
       i = ifeffit('set ework1 = floor(dat.energy)')
       i = ifeffit('set ework2 = ceil(dat.energy)')
       i = ifeffit('set eout1  = floor(dat.energy) - elow')
       i = ifeffit('set eout2  = ceil(dat.energy) + ehigh')

       i = ifeffit('set w.energy = range(ework1,ework2,egrid)')
       i = ifeffit('set o.e1     = range(eout1,ework1-egrid,egrid)')
       i = ifeffit('set o.e2     = range(ework2+egrid,eout2,egrid)')
       i = ifeffit('set o.energy = range(eout1,eout2,egrid)')

       i = ifeffit('set w.expdat = linterp(dat.energy,'//
     $      'dat.expdat,w.energy)')
       if (f2tof1) then 
cc          i  = ifeffit('set w.expdat = w.expdat * '//
cc     $         'w.energy/(max(1,e0))')
       endif
       npts = iffgetarr('w.energy', energy)
       npts = iffgetarr('w.expdat', expdat)

cc       print*,' npts =', npts
c
c generate initial tables of f' f'' on the same energy grid
       atz= iatz * 1.d0
       i = iffputsca('iz', atz)

       i = ifeffit('f1f2(energy=o.e1, z=iz)')

       i = ifeffit('set wid = core_width')
       if (ewidth.gt.0.d0) i = iffputsca('wid', ewidth)

       i =  ifeffit('f1f2(energy=w.energy, z=iz,width=wid)')
c       i=ifeffit('pre_edge(w.energy, w.f2, e0=e0)')
c       i=ifeffit('set cl_step = edge_step')
c       i=ifeffit('set cl_0 = pre_slope*floor(w.energy)+pre_offset')
c
c       i=ifeffit('pre_edge(w.energy, w.expdat, e0=e0)')
c
c       i=ifeffit('set dt_step = edge_step')
c       i=ifeffit('set dt_0 = pre_slope*floor(w.energy)+pre_offset')
c
c       i=ifeffit('set w.expscal = (cl_0-dt_0)+'//
c     $      ' w.expdat*cl_step/dt_step')

       i=ifeffit('set w.expscal =w.expdat')
       print*, ' hello expdat is simple! ' 
c

       npts = iffgetarr('w.expscal', expdat)

       i=ifeffit('f1f2(energy=o.e1, z=iz,width=wid)')
       i=ifeffit('set o.f1_low = o.f1')
       i=ifeffit('set o.f2_low = o.f2')
       
       i = ifeffit('f1f2(energy=o.e2, z=iz,width=wid)')
       i=ifeffit('set o.f1_high = o.f1')
       i=ifeffit('set o.f2_high = o.f2')


c broaden f1cl and f2cl
       i=iffgetarr('w.f1', f1cl)
       i=iffgetarr('w.f2', f2cl)
       i=ifeffit('set w.f1_cl = w.f1')
       i=ifeffit('set w.f2_cl = w.f2')
c
c align/shift/scale improved f''  to tabulated f''(df2 = expdat - f2cl)
       call chrdmp(' Matching experimental data to Cromer-Libermann ')
       if (f2tof1) then 
          call messag(' f'''' ')
       else
          call messag(' f'' ')
       end if

c$$$       call dkfit
c$$$       call dkfcn(npts,numvar,xvarys,df2,ierr)
          
       
       do 20 i = 1, npts
          df2(i) = -f2cl(i) +  expdat(i)
c     f(i) = -f2cl(i) + x(2) + x(3) * fx
c     $            + e * (x(4) + x(5)  * e )
 20      continue 

c do kk transform of delta f''  -> delta f'
       call messag(' Doing difference Kramers-Kronig transform')
       if (f2tof1) then
          call kkmclr(npts, energy, df2, df1)
       else
          call kkmclf(npts, energy, df2, df1)
       endif
c add delta f' to initial f', delta f''  to initial f''
       do 100 i = 1, npts
          o1(i)  = df1(i) + f1cl(i)
          o2(i)  = df2(i) + f2cl(i)
 100   continue 
       i = iffputarr('w.f1', npts, o1)
       i = iffputarr('w.f2', npts, o2)
c
c construct output on wider range
       i = ifeffit('set o.f1_cl = '//
     $      'join(join(o.f1_low, w.f1_cl),o.f1_high)')
       i = ifeffit('set o.f1 = '//
     $      'join(join(o.f1_low, w.f1),o.f1_high)')

       i = ifeffit('set o.f2_cl = '//
     $      'join(join(o.f2_low, w.f2_cl),o.f2_high)')
       i = ifeffit('set o.f2 = '//
     $      'join(join(o.f2_low, w.f2),o.f2_high)')


c play with docs (put user titles at top,
c      keep as many old doc lines as fit)
       i = iffputstr('doc', title(1))

       if (active)  then
          call messag(' ')
          call messag('  Ready to write out data file:')
          call askstr('** output file name',outfil)
       end if

       write(str,'(3a)')  'write_data(file=',
     $      outfil(1:istrln(outfil)),
     $      ',o.energy,o.f1,o.f2,o.f1_cl,o.f2_cl,$doc)'

       i =  ifeffit(str)
       i =  ifeffit('save diffkk.sav')
       call messag(' writing summary to diffkk.log')
       call dklog
       call messag(' -- diffkk done -- ')
 999   continue 
       end
