       subroutine iff_rename(str)
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
c  rename an array or scalar values for ifeffit
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       save

       character*(*) str, s1*256,  s2*256
       integer nwrds, iw, i, k1, k2, istrln
       external istrln

       nwrds = mkeys
       call bwords(str,nwrds,keys)

c check if this is a vector or scalar
       do 300 iw = 1, nwrds, 2
          k1 = istrln(keys(iw  ))
          k2 = istrln(keys(iw+1))
          s1 = keys(iw  )(1:k1)
          s2 = keys(iw+1)(1:k2)
          call lower(s1)
          call lower(s2)
          if (index(s1,'.').eq.0) then
             do 130 i = 1, maxsca
                if (s1.eq.scanam(i)) scanam(i) = s2
 130         continue 
          else
             do 150 i = 1, maxarr-1
                if (s1.eq.arrnam(i)) arrnam(i) = s2
 150         continue 
          end if
 300   continue 
c      
       return
       end

