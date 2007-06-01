       subroutine iff_uncert(str)
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
c
c purpose: propogate errors in fitted parameters to defined parameters
c 
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'feffit.h'
       save
       character*(*)  str
       character*128  xvar, yvar, out, pref, defkey(3)*64
       double precision xn, xcmin
       logical   lprint, lxall, lyall, lsave
       integer   ier, i, k, istrln, ndfkey, jx, jy, lx, ly
       external  istrln
c
cc       print*, ' error() ' 
       lprint = .false.
       lsave  = .true.
       call iff_sync
       call bkeys(str, mkeys, keys, values, nkeys)
       xvar = ' '
       pref = 'dx'
       out  = undef
       
c set default keywords for "undefined" sets
c ie: the first two unkeyed values will take keys 'x', 'y'
       ndfkey    = 0
       defkey(1) = undef
       do 100 i = 1, nkeys
cc          print*,i, keys(i)
          k = istrln( keys(i))
          if ((values(i).eq.undef).and.(i.le.ndfkey)) then
             values(i) = keys(i)
             keys(i)   = defkey(i)
          end if
          if (keys(i).eq.'prefix') then
             pref = values(i)
             call lower(pref)
          elseif (keys(i).eq.'min') then
             call str2dp(values(i), xcmin, ier)
          elseif (keys(i).eq.'print') then
             call str2lg( values(i), lprint, ier)
          elseif (keys(i).eq.'save') then
             call str2lg( values(i), lsave, ier)
          elseif (keys(i).eq.'no_save') then
             call str2lg( values(i), lsave, ier)
             lsave = .not.lsave
          else
             messg = keys(i)(1:k)//' " will be ignored'
             call warn(1,' *** correl: unknown keyword " '//messg)
          end if
 100   continue
c
c get/resolve array names
       jx = 0
       jy = 0
       
cc       print*, nvarys

       return
c end  subroutine iff_uncert
       end
