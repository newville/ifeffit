       subroutine iff_getpath(str)
c
c//////////////////////////////////////////////////////////////////////
c Copyright (c) 1997--2000 Matthew Newville, The University of Chicago
c Copyright (c) 1992--1996 Matthew Newville, University of Washington
c
c Permission to use and redistribute the source code or binary forms of
c this software and its documentation, with or without modification is
c hereby granted provided that the above notice of copyright, these
c terms of use, and the disclaimer of warranty below appear in the
c source code and documentation, and that none of the names of The
c University of Chicago, The University of Washington, or the authors
c appear in advertising or endorsement of works derived from this
c software without specific prior written permission from all parties.
c
c THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
c EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
c MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
c IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
c CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
c TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
c SOFTWARE OR THE USE OR OTHER DEALINGS IN THIS SOFTWARE.
c//////////////////////////////////////////////////////////////////////
c
c purpose: convert feff path to accessible program data
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'fefdat.h'
       include 'feffit.h'
       include 'pthpar.h'

       save
       character*(*)  str
       character*512 name1, defkey(2)*64, s
       character*8   suffix(mpthpr)
       double precision  tmpr(maxpts), tmpi(maxpts), r1, getsca, rx
       integer   ipath, idata, ier, i ,k, istrln, ndfkey, il
       integer   iff_eval, jx,jy, u2ipth, inpath, jfeff
       integer   xafs_path, ret, nx
       logical   do_arrs
       external  iff_eval, u2ipth, xafs_path
       external  getsca, istrln
       data suffix  /'s02', 'e0', 'ei', 'delr', 'sigma2', 'third',
     $      'fourth', 'degen' , 'k', 'amp', 'phase', 'file',
     $      ' ', ' ', ' ' , ' '/

c
c get default values for pre-edge parameters from current scalar values
       do_arrs = .false.
       call iff_sync
       call bkeys(str, mkeys, keys, values, nkeys)
       ipath = -1
       idata =  1 
       name1 = undef
c set default keywords for "undefined" sets
c ie: the first two unkeyed values will take keys 'energy', 'xmu'

       ndfkey    = 2
       defkey(1) = 'path'
       defkey(2) = 'prefix'
       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if ((values(i).eq.undef).and.(i.le.ndfkey)) then
             values(i) = keys(i)
             keys(i)   = defkey(i)
          end if
          if ( (keys(i).eq.'prefix') .or.
     $         (keys(i).eq.'group')) then
             name1 = values(i)
          elseif (keys(i).eq.'path') then
             call str2in(values(i), ipath, ier)
          elseif (keys(i).eq.'do_arrays') then
             call str2lg(values(i), do_arrs, ier)
          elseif (keys(i).eq.'data_set') then
             call str2in(values(i), idata, ier)
          else
             messg = keys(i)(1:k)//' " will be ignored'
             call warn(1,' *** get_path: unknown keyword " '//messg)
          end if
 100   continue
c
       if (ipath .le.0) return
       call fefinp
       ret    = xafs_path(ipath, tmpr, tmpi, rx)
       inpath = u2ipth(ipath)
       jfeff  = jpthff(inpath)

       call undels(name1)
       if (name1.eq.undef) then
          write(name1,'(a,i3.3)') 'path', ipath
       endif
       il = istrln(name1)
       s  = name1(1:il)//'_s02'
       call setsca(s, param(jfps02))
       s  = name1(1:il)//'_e0'
       call setsca(s, param(jfpe0))
       s  = name1(1:il)//'_ei'
       call setsca(s, param(jfpei))
       s  = name1(1:il)//'_delr'
       call setsca(s, param(jfpdr))
       s  = name1(1:il)//'_sigma2'
       call setsca(s, param(jfpss2))
       s  = name1(1:il)//'_third'
       call setsca(s, param(jfp3rd))
       s  = name1(1:il)//'_fourth'
       call setsca(s, param(jfp4th))
       s  = name1(1:il)//'_degen'
       call setsca(s, param(jfpdeg))
       s  = name1(1:il)//'_reff'
       r1 = getsca('reff',1)
       call setsca(s, r1)

       s  = name1(1:il)//'_lab'
       call settxt(s, pthlab(inpath))
       s  = name1(1:il)//'_id'
       call settxt(s, fpthid(jfeff))
       s  = name1(1:il)//'_file'
       call settxt(s, feffil(jfeff))

c
c generate ifeffit arrays from feff.dat columns
       if (do_arrs) then
          nx  = nffpts(jfeff)
          call set_array('k',     name1, qfeff(1,jfeff), nx, 1)
          call set_array('amp',   name1, theamp(1,jfeff), nx, 1)
          call set_array('phase', name1, thepha(1,jfeff), nx, 1)
          call set_array('caps',  name1, thcaps(1,jfeff), nx, 1)
          call set_array('rep',   name1, realp(1,jfeff), nx, 1)
          call set_array('lambda',name1, xlamb(1,jfeff), nx, 1)
       endif

       return
c end  subroutine iff_test
       end





