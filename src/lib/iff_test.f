       subroutine iff_test(str)
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
c purpose: ifeffit test function
c 
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'spline.h'
       save
       character*(*)  str
       character*128  xarr, yarr, name1, defkey(3)*64
       double precision  arr_x(maxpts), arr_y(maxpts)
       logical   stfind, eefind
       integer   jen, jpre, jxmu, ier, i, k, istrln, jdot
       integer   ndfkey, npts_y, npts_x, sort_xy
       integer  iff_eval, jx,jy
       external iff_eval, sort_xy
       external istrln
c
c get default values for pre-edge parameters from current scalar values
       call iff_sync
       call bkeys(str, mkeys, keys, values, nkeys)

c set default keywords for "undefined" sets
c ie: the first two unkeyed values will take keys 'energy', 'xmu'
       ndfkey    = 2
       defkey(1) = 'x'
       defkey(2) = 'y'
       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if ((values(i).eq.undef).and.(i.le.ndfkey)) then
             values(i) = keys(i)
             keys(i)   = defkey(i)
          end if
          if (keys(i).eq.'group') then
             name1 = values(i)
          elseif (keys(i).eq.'x') then
             xarr = values(i)
             call lower(xarr)
          elseif (keys(i).eq.'y') then
             yarr = values(i)
             call lower(yarr)
          else
             messg = keys(i)(1:k)//' " will be ignored'
             call warn(1, ' *** test: unknown keyword " '//messg)
          end if
 100   continue
c
c get/resolve array names
       if (name1.eq.undef) then
          jdot = index(xarr,'.')
          if (jdot.ne.0) name1 = xarr(1:jdot-1)
       endif
       if (name1.eq.undef) then
          jdot = index(xarr,'.')
          if (jdot.ne.0) name1 = yarr(1:jdot-1)
       endif
       if (name1.eq.undef) then
          call warn(2, ' test: can''t determine group name')
          return             
       endif
       call fixnam(name1,1)
       call lower(name1)
       jdot = istrln(name1)

       jx = iff_eval(xarr, name1, arr_x, npts_x)
       jy = iff_eval(yarr, name1, arr_y, npts_y)

       ier = sort_xy(arr_x, arr_y, npts_x,1.d-5)
       
c       do i = 1, npts_x
c          print*, i, arr_x(i), arr_y(i)
c       enddo

       return
c end  subroutine iff_test
       end

