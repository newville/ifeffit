       subroutine iff_color(str)
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
c  update the color table 
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'ifplot.h'
       save

       character*(*) str, s*512
       integer nwrds,  i, j, ier, jcol, istrln, iff_eval_in
       external istrln, iff_eval_in

       s = str
       nwrds = mkeys
       call bwords(s,nwrds,keys)
c 
cc       print*, "  bg/fg/gr = ", mcolbg, mcolfg, mcolgr
cc       print*, "  bg/fg/gr = ", pltcol(mcolbg), pltcol(mcolfg),
cc     $      pltcol(mcolgr)
       call rmquot(keys(1)) 
       if (keys(1).eq.'show') then
          call echo(' plot color table: ')
          call echo('    bg   = '//pltcol(mcolbg) )
          call echo('    fg   = '//pltcol(mcolfg) )
          call echo('    grid = '//pltcol(mcolgr) )
          do 100 j = 1, mctabl-3
             if (pltcol(j).ne.undef) then
                write(messg,'(3x,i5,2a)') j,' = ',pltcol(j)
                call echo(messg)
             end if
 100      continue 
       else 
          do 300 i = 1, nwrds, 2
             ier = 0
             call str2in(keys(i),jcol,ier)
             call rmquot(keys(i+1))
             if (ier.eq.0) then 
                call setcol(jcol, keys(i+1) )
             elseif ((keys(i).eq.'bg') .or.
     $               (keys(i).eq.'background')) then
                call setcol(mcolbg, keys(i+1) )
             elseif ((keys(i).eq.'fg') .or.
     $               (keys(i).eq.'foreground')) then
                call setcol(mcolfg, keys(i+1) )
             elseif (keys(i)(1:2).eq.'gr') then
                call setcol(mcolgr, keys(i+1) )
             end if
 300      continue 
       endif
c      
       return
       end

       subroutine iff_pstyle(str)
c
c  update the linestyle table 
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'ifplot.h'
       save
       character*(*) str, s*512
       integer nwrds,  i, j, ier, jcol, istrln, iff_eval_in
       external istrln, iff_eval_in
       s     = str
       nwrds = mkeys
       call bwords(s,nwrds,keys)
c 
       call rmquot(keys(1))       
       if (keys(1).eq.'show') then
          call echo(' plot style table: ')
          do 100 j = 1, maxplt
             if (pltsty(j).ne.undef) then
                write(messg,'(3x,i5,2a)') j,' = ',pltsty(j)
                call echo(messg)
             end if
 100      continue 
       else 
          do 300 i = 1, nwrds, 2
             ier = 0
             call str2in(keys(i),jcol,ier)
             call rmquot(keys(i+1))
             call set_plsty(keys(i+1), isty(jcol), pltsty(jcol))
 300      continue 
       endif
c      
       return
       end

       subroutine set_plsty(style, index, out)
c
c purpose: set PGPLOT lines style based on style name

c arguments:
c      style   string                       [in]
c      index   style index to use           [out]
c      out     'normalized style string'    [out]
c
c  copyright (c) 2000  matt newville
       character*(*) style, sty*32, out*32
       integer  index, itmp, ier

       index = 1
       out   = 'solid'
       sty   = style
       call triml(sty)
       call lower(sty)
       if ((sty.eq.'solid').or.(sty.eq.'lines')) then
          index = 1
          out   = 'solid'
       elseif ((sty(1:4).eq.'dash')) then
          index = 2
          out   = 'dashed'
       elseif (sty(1:6).eq.'dot-da') then
          index = 3
          out   = 'dot-dashed'
       elseif ((sty.eq.'dotted').or.(sty.eq.'dot')) then
          index = 4
          out   = 'dotted'
       elseif (sty(1:11).eq.'linespoints') then
          call str2in(sty(12:), itmp, ier)
          if (itmp.lt.0) itmp = 1
          index = itmp + 5
          out   = style
       elseif (sty(1:6).eq.'points') then
          call str2in(sty(7:), itmp, ier)
          if (itmp.lt.0) itmp = 1
          index = -itmp
          out   = style
       end if
       return
       end
c
      
       subroutine setcol(jcol, string)
c
c purpose: set color in the color table to a named color
c
c arguments:
c      jcol    color index to set                       [in]
c      string  string to interpret as color             [in]
c
c notes:
c   1.  string can either be a valid colorname from rgb.txt
c       or begin with '#', and then be interpreted as a 
c       hexadecimal rgb specification
c
c requires:  hexcol, pgscrn
c
c  copyright (c) 1998  matt newville
c
       include 'consts.h'
       include 'ifplot.h'
       save
       character*(*) string, s*32
       integer jcol, ier
       if ((jcol.le.mctabl).and.(jcol.ge.0)) then
          ier = 0
          s = string
          call lower(s)
cc          print*, ' setcol: ', jcol, s
         if (s(1:1).eq.'#') then
             call hexcol(jcol, s(2:))
          else
             call pgscrn(jcol, s, ier)
          end if
          if (ier.eq.0) then
             pltcol(jcol) = s
          else
             call pgscrn(jcol, pltcol(1), ier)
          end if
       end if
       if (jcol .eq. mcolbg) then
           pltcol(0) = pltcol(mcolbg)
          call pgscrn(0, pltcol(0), ier)
       endif
       return
       end
       subroutine getcol(string,jcol)
c
c purpose: lookup a named color in the color table.  if not found, and 
c          there's room, add to color table
c
c arguments:
c      string  string to interpret as color             [in]
c      jcol    color index to set                       [out]
c
c notes:
c   1.  string can either be a valid colorname from rgb.txt
c       or begin with '#', and then be interpreted as a 
c       hexadecimal rgb specification
c
c requires:  hexcol, pgscrn
c
c  copyright (c) 1998  matt newville
c
       include 'consts.h'
       include 'ifplot.h'
       save
       character*(*) string, s*32
       integer jcol,  j

       s = string
       call lower(s)
       jcol = 0
       do 10 j = 0, mctabl
          if (pltcol(j).eq.s) then 
             jcol = j
             return
          elseif (pltcol(j).eq.undef) then
             jcol = j
             call setcol(j,s)
             return
          elseif (j.eq.mctabl) then
             call echo(' ** ifeffit plot: color table full ')
             call warn(1,
     $            ' **    redefine some colors with color command')
          end if
 10    continue 
       return
       end
       subroutine hexcol(jcol, string)
c
c purpose: set pgplot color from a hexdecimal string
c
c arguments:
c      string  string to interpret as color             [in]
c
c
c  copyright (c) 1998  matt newville
       integer i, jcol, ilen, istrln, fact
c  note:  uses single precision!
       real    r, g, b, total, zero
       character*(*) string, s*32, digs*16
       parameter (digs = '0123456789abcdef',fact=16, zero=0)
       total = 1
       s = string
       if (s(1:1) .eq. '#') s = s(2:)
       ilen = istrln(s)
       call lower(s)
       r    = zero
       g    = zero
       b    = zero
       idig = ilen / 3
       do 10  i = 1, idig
          total = total * fact
          r  = fact * r -1 + max(1, index(digs, s(i:i)))
          g  = fact * g -1 + max(1, index(digs, s(i+idig:i+idig)))
          b  = fact * b -1 + max(1, index(digs, s(i+2*idig:i+2*idig)))
 10    continue 
       r = r / total
       g = g / total
       b = b / total
cc       print*, ' hexcol : pgscr ( ', jcol, r, g, b, ' ) '
       call pgscr(jcol,r,g,b)
       return
       end
