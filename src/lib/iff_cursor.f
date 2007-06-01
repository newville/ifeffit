       subroutine iff_cursor(argu)
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
c  get cursor position of plot:
c  uses pgpplot routine pgband
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'ifplot.h'
       save

       character c*1, str*32, argu*(*)
c note: pgcurs uses single precision!
       real      rx, ry, ox, oy
       double precision d_x, d_y, getsca
       logical   do_show
       integer   iff_eval_in, i, ier, k
       integer   pgband, iret, ilen, istrln, mode, iposn
       external  pgband, istrln, getsca, iff_eval_in
       c     = ' '

       str = argu
       call bkeys(str, mkeys, keys, values, nkeys)
       mode = 0
       iposn= 0
       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if (keys(i).eq.'show') then
             do_show = .true.
          elseif (keys(i).eq.'mode') then
             ier = iff_eval_in(values(i),mode)
          elseif (keys(i).eq.'last_pos') then
             ier = iff_eval_in(values(i),iposn)
          elseif ((keys(i).eq.'cross-hair') .or.
     $            (keys(i).eq.'cross_hair') .or.
     $            (keys(i).eq.'crosshair') ) then
             mode = 7
          elseif (keys(i).eq.'vert') then
             mode = 6  
          elseif (keys(i).eq.'horiz') then
             mode = 5
          elseif (keys(i).eq.'xrange') then
             mode = 4
          elseif (keys(i).eq.'yrange') then
             mode = 3
          else
             messg = keys(i)(1:k)//' " will be ignored'
             call warn(1,' *** cursor: unknown keyword " '//messg)
          end if
 100   continue
c  grwin specific: raise to top
       call iff_plotraise(1)
       ox   = real(getsca('cursor_x',1))
       oy   = real(getsca('cursor_y',1))
       call echo(' select cursor position')
       iret = pgband(mode, iposn, ox, oy, rx, ry, c)
       if (iret.eq.1) then
          call setsca('cursor_x', dble(rx))
          call setsca('cursor_y', dble(ry))
       end if

       if (do_show) then
          write(tmpstr,'(1x,a,g15.6,a,g15.6)') 'cursor: x = ', rx,
     $         ', y = ', ry
          call echo('  '//tmpstr)
       endif
       return
c  end subroutine cursor
       end

       subroutine iff_zoom(argu)
c
c zoom in on plot
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'ifplot.h'
       save
       integer  i, ilen, istrln, k
       integer  pgband, iret, mode1, mode2
       logical  do_show
       real      rx1, rx2, ry1, ry2 
       double precision  getsca
       character*32 str, s, argu*(*), c*1
       external istrln, pgband, getsca
c
       mode1 = 7
       mode2 = 2
       str = argu

       call bkeys(str, mkeys, keys, values, nkeys)

       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if (keys(i).eq.'show') then
             do_show = .true.
          elseif (keys(i).eq.'nobox') then
             mode1 = 0
             mode2 = 0
          else
             messg = keys(i)(1:k)//' " will be ignored'
             call warn(1,' *** zoom: unknown keyword " '//messg)
          end if
 100   continue


c  get first point with pgband mode=0
       rx2  = real(getsca('cursor_x',1))
       ry2  = real(getsca('cursor_y',1))
c  grwin specific: raise to top
       call iff_plotraise(1)

       call echo(' select cursor position')
       iret = pgband(mode1, 0, rx2, ry2, rx1, ry1, c)
       rx2  = rx1
       ry2  = ry1
c  now use specified mode
       iret = pgband(mode2, 0, rx1, ry2, rx2, ry2, c)
       call setsca('cursor_x', dble(rx2))
       call setsca('cursor_y', dble(ry2))

       xlim(1) = dble(min(rx1,rx2))
       xlim(2) = dble(max(rx1,rx2))
       xlim(3) = dble(min(ry1,ry2))
       xlim(4) = dble(max(ry1,ry2))
       do 200 i = 1, 4
          limits(i) = .true.
 200   continue 
       write(tmpstr,'(1x,a,g15.6)') 'xmin= ', xlim(1)
       call iff_plot(tmpstr)
c
       if (do_show) then
          write(tmpstr,'(1x,a,g15.6,a,g15.6)') 'cursor: x = ', xlim(1),
     $         ', y = ', xlim(3)
          call echo('  '//tmpstr)
          write(tmpstr,'(1x,a,g15.6,a,g15.6)') 'cursor: x = ', xlim(2),
     $         ', y = ', xlim(4)
          call echo('  '//tmpstr)
       endif
c      
       return
       end

       subroutine iff_plotmarker(str)
c
c  plot a single marker point at an x/y position
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'ifplot.h'
       save
       character c*1, str*(*)
c note: pgcurs uses single precision!
       real      rx, ry,xx,yy
       character*64  defkey(3), color
       double precision  getsca
       integer   iff_eval_re, iff_eval_in, i, ier, k, ij
       integer   idfkey, ndfkey,j, jtcol
       integer   iret, ilen, istrln, mode, iposn
       external  istrln, getsca, iff_eval_re, iff_eval_in

       call bkeys(str, mkeys, keys, values, nkeys)

       defkey(1) = 'x'
       defkey(2) = 'y'
       defkey(3) = 'marker'
       idfkey    = 1
       ndfkey    = 3
       tmpstr    = undef
       color     = pltcol(mcolfg)

       ij        = -1001
       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if (keys(i).eq.'clear') then
             npmark = 0
             do 88 j = 1, mpmark
                imarker(j) = -13000
 88          continue 
          else if ((values(i).eq.undef).and.
     $         (idfkey.le.ndfkey).and.(i.le.5)) then
             values(i) = keys(i)
             keys(i)   = defkey(idfkey)
             idfkey    = idfkey + 1
          end if
          if (keys(i).eq.'x') then
             ier = iff_eval_re(values(i),xx)
          elseif (keys(i).eq.'y') then
             ier = iff_eval_re(values(i),yy)
          elseif (keys(i).eq.'marker') then
             ier = iff_eval_in(values(i),ij)
          elseif (keys(i).eq.'clear') then
             npmark = 0
          elseif (keys(i).eq.'color') then
             color = values(i)
          else
             messg = keys(i)(1:k)//' " will be ignored'
             call warn(1,' *** plot_marker: unknown keyword " '//messg)
          end if
 100   continue

       if (ij .ge. -1000) then 
          npmark = npmark + 1
          imarker(npmark) = ij
          xmarks(npmark)  = xx
          ymarks(npmark)  = yy
          call getcol(color,jtcol)
          mrkcol(npmark) = jtcol
       end if
       call iff_plot(' ')

       return
c  end subroutine plot_marker
       end

       subroutine iff_plotarrow(str)
c
c  plot an arrow
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'ifplot.h'
       save
       character c*1, str*(*)
c note: pgcurs uses single precision!
       real      x1,x2,y1,y2, barb,angle, size, xx(2),yy(2)
       character*64  defkey(5),color
       integer   iff_eval_re, i, ier, k, ij, j, jtcol
       integer   idfkey, ndfkey, ixarr
       integer   iret, ilen, istrln, mode, iposn
       external  istrln, iff_eval_re

       call bkeys(str, mkeys, keys, values, nkeys)

       defkey(1) = 'x1'
       defkey(2) = 'y1'
       defkey(3) = 'x2'
       defkey(4) = 'y2'
       angle     = 45
       barb      = 0.4
       size      = 2
       idfkey    = 1
       ndfkey    = 4
       ixarr     = 1
       color = pltcol(mcolfg)
       
       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if (keys(i).eq.'clear') then
             nparro = 0
             do 88 j = 1, mparro
                iarrow(i) = -1
 88          continue 
          elseif ((values(i).eq.undef).and.
     $         (idfkey.le.ndfkey).and.(i.le.5)) then
             values(i) = keys(i)
             keys(i)   = defkey(idfkey)
             idfkey    = idfkey + 1
          end if
          if (keys(i).eq.'x1') then
             ier = iff_eval_re(values(i),x1)
          elseif (keys(i).eq.'y1') then
             ier = iff_eval_re(values(i),y1)
          elseif (keys(i).eq.'x2') then
             ier = iff_eval_re(values(i),x2)
          elseif (keys(i).eq.'y2') then
             ier = iff_eval_re(values(i),y2)
          elseif (keys(i).eq.'y2') then
             ier = iff_eval_re(values(i),y2)
          elseif (keys(i).eq.'barb') then
             ier = iff_eval_re(values(i),barb)
          elseif (keys(i).eq.'angle') then
             ier = iff_eval_re(values(i),angle)
          elseif (keys(i).eq.'color') then
             color = values(i)
          elseif (keys(i).eq.'size') then
             ier = iff_eval_re(values(i),size)
          elseif (keys(i).eq.'no_head') then
             ixarr= -1
          elseif (keys(i).eq.'fill') then
             ixarr= 1
          elseif (keys(i).eq.'outline') then
             ixarr= 2
          elseif (keys(i).eq.'clear') then
             nparro = 0
             ixarr = -100
          else
             messg = keys(i)(1:k)//' " will be ignored'
             call warn(1,' *** plot_arrow: unknown keyword " '//messg)
          end if
 100   continue

       if (ixarr .ge. -10) then 
          nparro = nparro + 1
          iarrow(nparro)   = ixarr
          xarros(nparro,1) = x1
          xarros(nparro,2) = y1
          xarros(nparro,3) = x2
          xarros(nparro,4) = y2
          xarros(nparro,5) = angle
          xarros(nparro,6) = barb
          xarros(nparro,7) = size
          call getcol(color,jtcol)
          xarros(nparro,8) = jtcol
       endif
       call iff_plot(' ')

       return
c  end subroutine plot_arrow
       end

       subroutine iff_plottext(str)
c
c  plot a single marker point at an x/y position
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'ifplot.h'
       save
       character c*1, str*(*)
c note: pgcurs uses single precision!
       real      xx, yy
       character*64  defkey(5)
       double precision  getsca
       integer   iff_eval_re, iff_eval_in, i, ier, k, ij
       integer   idfkey, ndfkey,j
       integer   iret, ilen, istrln, mode, iposn
       external  istrln, getsca, iff_eval_re, iff_eval_in
       

       call rmquot(str)
       call bkeys(str, mkeys, keys, values, nkeys)
       defkey(1) = 'x'
       defkey(2) = 'y'
       defkey(3) = 'text'
       idfkey    = 1
       ndfkey    = 3
       tmpstr    = undef

       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if (keys(i).eq.'clear') then
             do 88 j = 1, nplabs
                pltlab(j) = undef
 88          continue 
          endif
          if ((values(i).eq.undef).and.
     $         (idfkey.le.ndfkey).and.(i.le.5)) then
             values(i) = keys(i)
             keys(i)   = defkey(idfkey)
             idfkey    = idfkey + 1
          end if
          if (keys(i).eq.'x') then
             ier = iff_eval_re(values(i),xx)
          elseif (keys(i).eq.'y') then
             ier = iff_eval_re(values(i),yy)
          elseif (keys(i).eq.'text') then
             tmpstr = values(i)
          elseif (keys(i).eq.'clear') then
             nplabs = 0
          elseif (keys(i).eq.'size') then
             ier = iff_eval_re(values(i), txtsiz)
          else
             messg = keys(i)(1:k)//' " will be ignored'
             call warn(1,' *** plot_text: unknown keyword " '//messg)
          end if
 100   continue

c plot text 
       if (tmpstr .ne. undef) then
          nplabs = nplabs + 1
          call rmquot(tmpstr)
          pltlab(nplabs) = tmpstr
          xplabs(nplabs) = xx
          yplabs(nplabs) = yy
       end if
       call iff_plot(' ')
       return
c  end subroutine cursor
       end
