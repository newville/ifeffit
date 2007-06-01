       subroutine iff_random(str)
c
c//////////////////////////////////////////////////////////////////////
c Copyright (c) 2004 Matthew Newville, The University of Chicago
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
       save
       character*(*)  str
       character*128 oname, name1, name2, defkey(3)*64, rdist*16
       integer  ier, i, k, istrln, npts, jdot, ndfkey, iseed
       integer  iff_eval, iff_eval_in, iff_eval_dp
       double precision randmt, gauss_rand, normal_rand
       double precision sigma
       external iff_eval, iff_eval_in, iff_eval_dp
       external randmt, gauss_rand, normal_rand
       external istrln
c
       rdist  = 'uniform'
       oname  = undef
       npts   =   0
       sigma  = one
       call iff_sync
       call bkeys(str, mkeys, keys, values, nkeys)

c set default keywords for "undefined" sets
c ie: the first two unkeyed values will take keys 'energy', 'xmu'
       ndfkey    = 2
       defkey(1) = 'output'
       defkey(2) = 'npts'
       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if ((values(i).eq.undef).and.(i.le.ndfkey)) then
             values(i) = keys(i)
             keys(i)   = defkey(i)
          end if
          if (keys(i).eq.'npts') then
             ier = iff_eval_in(values(i), npts)
          elseif (keys(i).eq.'output') then
             oname = values(i)
             call lower(oname)
          elseif (keys(i).eq.'dist') then
             rdist = values(i)
             call lower(rdist)
          elseif (keys(i).eq.'sigma') then
             ier = iff_eval_dp(values(i), sigma)
          elseif (keys(i).eq.'seed') then
             ier = iff_eval_in(values(i), iseed)
             if (ier.eq.0) then
                if (iseed.eq.0) ier = 4357
                call seed_randmt(iseed)
             endif
          else
             messg = keys(i)(1:k)//' " will be ignored'
             call warn(1,' *** random: unknown keyword " '//messg)
          end if
 100   continue
c
c get/resolve array names
       jdot = index(oname,'.')
       if ((jdot.eq.0).or.(oname.eq.undef)) then
          call warn(2, ' random: no output array named')
          return             
       endif
       name1 = oname(1:jdot-1)
       name2 = oname(jdot+1:)

       call lower(name1)
       call lower(name2)
       call fixnam(name1,1)
       call fixnam(name2,2)
       jdot = istrln(name1)

       npts = min(npts,maxpts)

       call lower(rdist)
       do 200 i = 1, npts
          tmparr(i) = zero
 200   continue 

cc       print*, ' use distribution ', rdist(:12)
       if (rdist(:5).eq.'gauss') then
          do 300 i = 1, npts
             tmparr(i) = sigma*gauss_rand()
 300      continue 
       elseif (rdist(:6).eq.'normal') then
          do 400 i = 1, npts
             tmparr(i) = sigma*normal_rand()
 400      continue 
       else
          do 900 i = 1, npts
             tmparr(i) = sigma*randmt()
 900      continue 
       endif

       call set_array(name2,name1, tmparr,npts,1)

       return
c end  subroutine iff_random
       end

