      subroutine iff_plot_init(index)
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
c initialize common blocks for plotting in ifeffit
c       
       implicit none
       include 'consts.h'
       include 'ifplot.h'
       save
       character*64  pltdev, dstr*128, d1str*16, desc
       integer index, j, k, pgopen, ii, ndevx, inter, i
       integer tlen, dlen , istrln
       external  pgopen, istrln

cc       print*, 'iff_plot_init: index = ', index
       
       if ((index.eq.99) .and.(icurdev.gt.0)) call pgclos
       nplot  = 0
       do 90 j = 1, 4
          xlim(j)  = 0
          tlim(j)  = 0
          limits(j) = .false.
 90    continue 
       igrid     = 1
       inplot    = 0
       nplabs    = 0
       ipgwin(4) = 0
       ipgwin(3) = 0
       ipgwin(2) = 0
       ipgwin(1) = 0
       icurwin   = 1
       do 100 j = 1, mplabs
          pltlab(j) = undef
          xplabs(j) = 0
          yplabs(j) = 0
 100   continue 
       npmark    = 0
       do 105 j = 1, mpmark
          imarker(j) = -13000
          xmarks(j)  = 0
          ymarks(j)  = 0
          mrkcol(j)  = 0
 105   continue 

       do 130 j = 1, mpdevs
          ipgdev(j) = -1
          pgdevs(j) = ' '
 130   continue 
       call gettxt('plot_device',pltdev)

       do 240 j = 1, maxplt
          icol(j)   = j
          isty(j)   = 1
          pltsty(j) = undef
          pltkey(j) = ''
          do 220 k = 1, maxpts
             plot_x(k,j) = 0
             plot_y(k,j) = 0
 220      continue 
 240   continue 

       call settxt('plot_file', ' ')
       do 265 j = 1, mctabl
          pltcol(j) = undef
 265   continue 
c
       pltcol(0)      = 'white'
       pltcol(mcolbg) = 'white'
       pltcol(mcolfg) = 'black'
       pltcol(mcolgr) = '#CCBEE0'
       pltcol( 1) =  'blue'
       pltcol( 2) =  'red'
       pltcol( 3) =  'darkgreen'
       pltcol( 4) =  'black'
       pltcol( 5) =  'magenta'
       pltcol( 6) =  'maroon'
       pltcol( 7) =  'yellow'
       pltcol( 8) =  'orange'
       pltcol( 9) =  'purple'
       pltcol(10) =  'grey77'
       do 285 j = 1, 8
          pltsty(j) = 'solid'
 285   continue 

       ilnwid =  2
       ilnsty =  1
       ichrfn =  1
       axisiz =  1.5
       txtsiz =  1.5
       mkrsiz =  1.5
       
c  stuff for "real reset and starting plot device"
       if (index.eq.0)  then
          ii = 16
          ndevx = 0
          call pgqndt(ndevx)
          dstr = ' '
          k    = 1
          do 800 i = 1, ndevx
             call pgqdt(i,d1str,tlen,desc,dlen,inter)
             call triml(d1str)
             j    = istrln(d1str)
             dstr = dstr(1:k)//' '//d1str(2:j) 
             k    = istrln(dstr)
 800      continue 
          call triml (dstr)
          call settxt('plot_devices', dstr)

          call pgqinf('DEV/TYPE', pgdevs(1), ii)
          ipgdev(1) = pgopen(pltdev)
          icurdev   = ipgdev(1)
       else if (index.eq.-1) then
          icurdev = -1
       end if
c       
       return
       end
