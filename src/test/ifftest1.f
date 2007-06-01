       program ifftest1
c
c      basic testsuite of ifeffit functionality
       integer ifeffit, iffgetsca, iffputsca, iffgetarr
       integer i, j, ntest, ret, npts, npts2, nkeys
       parameter (mkeys=12)
       character*128 str1,  values(mkeys)
       character*64  keys(mkeys)

       double precision x, xarr(2048)
       external ifeffit, iffgetsca, iffputsca, iffgetarr

       print*, '==  IFEFFIT test suite: Fortran version == '
cc test 1:  load library
       j = 1
       print*, '=Test ',j,': load library'
       ret = ifeffit(' ')
       if (ret.ne.0) goto 500
       print*, '=Passed Test #', j
cc
cc test 2: 
       j = j+1 
       print*, '=Test ',j,': basic data types (scalar, array, string):'
c 2a: iffgetsca
       ret = ifeffit(' set x = 1.234')
       if (ret.ne.0) goto 500
       ret = iffgetsca('x',x)
       if (abs(x - 1.234d0).ge.1.d-7) goto 500
       print*, '    get scalar from ifeffit with iffgetsca OK'
c 2b: iffputsca
       x = 3.68d-2
       ret = iffputsca('yvar', x)
       if (ret.ne.0) goto 500
       ret = iffgetsca('yvar',x)
       if (abs(x - 3.68d-2).ge.1.d-7) goto 500
       print*, '    put scalar into ifeffit with iffputsca OK'
c 2c: iffgetarr
       ret = ifeffit(' set my.arr  = indarr(100)')
       ret = ifeffit(' set my.arr2 = sqrt(my.arr)')
       if (ret.ne.0) goto 500
       npts = iffgetarr('my.arr',xarr)
       if (npts.ne.100) goto 520
       if (abs(xarr(1) - 1.d0).ge.1.d-7) goto 500
       if (abs(xarr(2) - 2.d0).ge.1.d-7) goto 500
       if (abs(xarr(10) - 10.d0).ge.1.d-7) goto 500
       print*, '    get array  from ifeffit with iffgetarr OK'
c 2d: strings
       ret = ifeffit(' set $str = "i am a string"')
       ret = iffgetstr('str',str1)
ccc       print*, ' ret = ', ret, str1(1:13)
       if (str1(1:13).ne.'i am a string') goto 500
       print*, '    get string from ifeffit with iffgetstr OK'
       print*, '=Passed Test #', j

cc
cc test 3: 
       j = j+1 
       print*, '=Test ',j,': basic data manipulation'
       ret = ifeffit(' set x   = 5.0')
       ret = ifeffit(' def phi = (sqrt(x)-1) /2 ')
       ret = iffgetsca('phi',x)
       if (abs(x - 0.618033989).ge.1.d-7) goto 510

       ret = ifeffit(' set x = 4.0')
       ret = iffgetsca('phi',x)
       if (abs(x - 0.5).ge.1.d-7) goto 510

       print*, '    scalar manipulation  OK'

       ret  = ifeffit(' set my.arr  = indarr(100)')
       ret  = ifeffit(' def my.arr2 = sqrt(my.arr)')
       npts = iffgetarr('my.arr2',xarr)
       if (npts.ne.100) goto 520
       if (abs(xarr(2)   - sqrt(2.d0)).ge.1.d-7) goto 510
       if (abs(xarr(64)  -       8.d0).ge.1.d-7) goto 510

       ret  = ifeffit(' def my.arr2 = my.arr / 3.')
       npts = iffgetarr('my.arr2',xarr)
       if (npts.ne.100) goto 520
       if (abs(xarr(3)   -  1.d0).ge.1.d-7) goto 510
       if (abs(xarr(60)  - 20.d0).ge.1.d-7) goto 510

       print*, '    array manipulation  OK'
       print*, '=Passed Test #', j
cc
cc test 4: 
       j = j+1 
       print*, '=Test ',j,': datafile read/write'
       ret = ifeffit('read_data(file=cu.xmu, group=cu,'//
     $               'label="energy xmu")')
       npts  = iffgetarr('cu.energy',xarr)
       npts2 = iffgetarr('cu.xmu',xarr)
       if ((npts.ne.612) .or. (npts2.ne.612)) goto 520
       print*, '    read arrays from cu.xmu OK'

       ret = ifeffit('write_data(file=cu_out.xmu, cu.energy, '//
     $               'cu.xmu, $cu_title*)')
       if (ret.ne.0) goto 500
       print*, '    wrote cu_out.xmu OK'
       ret = ifeffit('read_data(file=cu_out.xmu, group=cu_out,'//
     $               'label="energy xmu")')
       npts  = iffgetarr('cu.energy',xarr)
       if ((npts.ne.612) .or. (npts2.ne.612)) goto 520
       print*, '    was able to re-read cu_out.xmu OK'
       print*, '=Passed Test #', j
cc
cc test 5: 
       j = j+1 
       print*, '=Test ',j,': commandline basics and macros'
       ret = ifeffit (' set (a = ')
       if (ret.ne.-1) goto 500
       ret = ifeffit (' 13)')
       if (ret.ne.0) goto 500
       ret = iffgetsca('a',x)
       if (abs(x - 13.d0).ge.1.d-7) goto 510
       print*, '    can do continuation line OK'
       ret = ifeffit (' macro xx ')
       if (ret.ne.-2) goto 500
       ret = ifeffit (' y_xx = 10.2 ')
       if (ret.ne.-2) goto 500
       ret = ifeffit (' end macro ')
       if (ret.ne.0) goto 500
       print*, '    can define a macro OK'
       ret = ifeffit (' set y_xx = 9 ')
       ret = ifeffit (' xx ')
       ret = iffgetsca('y_xx',x)
       if (abs(x - 10.2d0).ge.1.d-7) goto 510
       print*, '    can execute a macro OK'
cc
cc test 6: 
       j = j+1 
       print*, '=Test ',j, ': information retrieval' 
       print*, ' -->  "show @arrays": '
       ret = ifeffit (' show @arrays')
       if (ret.ne.0) goto 500

       print*, ' -->  "show @groups": '
       ret = ifeffit (' show @groups')
       if (ret.ne.0) goto 500
       print*, ' -->  "show @group=cu": '
       ret = ifeffit (' show @group=cu')
       if (ret.ne.0) goto 500       
       print*, ' -->  "show @scalars":'
       ret = ifeffit (' show @scalars')
       if (ret.ne.0) goto 500       
       
       print*, '=Passed Test #', j

cc
cc test 6: 
       j = j+1 
       print*, '=Test ', j,' : background subtraction '
       ret = ifeffit('spline(cu.energy, cu.xmu,rbkg=1.0,kweight=1)')
       if (ret.ne.0) goto 500
       ret = iffgetsca('rbkg',x)
       print*, ' rbkg ', x
       if (abs(x - 1.d0).ge.1.d-1) goto 510
       ret = iffgetsca('e0',x)
       print*, ' e0 ', x
       if (abs(x - 8977.d0).ge.3.d0) goto 510
       ret = iffgetsca('edge_step',x)
       print*, ' edge_step ', x
       if (abs(x - 2.12d0).ge.1.d-1) goto 510
c  check array sizes
       npts = iffgetarr('cu.bkg',xarr)
       if (npts.lt.400)  goto 520
       npts2 = iffgetarr('cu.xmu',xarr)
       if (npts2.ne.npts)  goto 520
c
       npts = iffgetarr('cu.k',xarr)
       if (npts.lt.400)  goto 520
       npts2 = iffgetarr('cu.chi',xarr)
       if (npts2.ne.npts)  goto 520
       ret  = ifeffit('show @arrays')
       print*, '    can do background subtraction OK'


c parse test

       str1 = 'x =13.214, File = B.dat, Verbose,'//
     $      ' sig = sqrt(A + min(b,c)) '
       
       nkeys = 9
       call bkeys(str1, mkeys, keys, values, nkeys)

       print*, '=Parse Test: parge arg list:'
       print*, ' arg = ', str1
       print*, 'nkeys = ', nkeys
       do i =1, nkeys
          print*, i, keys(i)(1:30)
          print*, values(i)(1:70)
       enddo

cc test : 
       j = j+1 
       print*, '=Final Test: can quit ifeffit'
       ret = ifeffit('quit')
       if (ret.ne.1) goto 500     
       print*, '=Passed Test #', j         
       print*, '==All Tests Passed.'
       go to 700
 500   continue 
       print*, '*** error: bad return value from ifeffit() ', ret
       goto 600
 510   continue
       print*, '*** error: bad numerical value '
       goto 600 
 520   continue 
       print*, '*** error: wrong number of points for array'
       goto 600 
 600   continue 
       print*, '==Failed Test # ', j
 700   continue
       end
