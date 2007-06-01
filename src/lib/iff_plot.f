      subroutine iff_plot(str)
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
c purpose: main plotting routine for ifeffit: uses pgplot
c
c arguments:
c      str     command line for ifeffit                 [in]
c
c notes:
c   1. use pgplot calls to make simple 2d plots of vectors
c
c requires:  everything in ifeffit and pgplot
c
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'ifplot.h'
       save
       integer  pgopen,  i, j, k, istrln, jc, inewp, ieras
       integer  itmp, idum, mpts, ipd, ix1, ix2, nofxsp, ier
c note the mixed precision: pgplot uses single precision
       real  tlo, thi,  xpltmp, ypltmp, ynkey, xnkey, scale_key
       real  xxkey(2), yykey(2), xx(2), yy(2)
       double precision xxarr(maxpts), dxarr(maxpts)
       double precision prange_min, getsca
       real  xkey_x0, xkey_y0, xkey_yd, xkey_y1, xkey_yy
       parameter (prange_min = 1.d-8)
       character*128  xfunc, yfunc, plotf, pltdev, pltfil
       character*128  xlabel, ylabel, plabel, ptitle, name1
       character*32   tmpcol, tmpsty, pkey*32, dxfunc, dyfunc
       character*(*) str, xboxs*10, yboxs*10
       character*64  defkey(3)
       integer   idfkey, ndfkey, ii, nx1, ny1, isarr, ikey, ilkey
       logical   newdev, doplot, show_key, err_bx, err_by
       logical   xinp,yinp
       integer   iff_eval, iff_eval_re, iff_eval_in
       external  iff_eval, iff_eval_re, iff_eval_in
       external  pgopen, nofxsp, istrln, getsca
c
cc       print*, '>>> ifplot : ', str(1:60)
cc       print*, '>>> ifplot nplot = ' , nplot
c
       if (icurdev.eq.-1) call iff_plot_init(0)
       doplot = .true.
       err_bx = .false.
       err_by = .false.
       xinp   = .false.
       yinp   = .false.
       xpltmp = 0
       ypltmp = 0
       pkey   = ''
       plabel = undef
       xfunc  = undef
       yfunc  = undef
       call gettxt('plot_device',pltdev)
       call triml(pltdev)
       if ((pltdev.eq.undef).or.(pltdev.eq.' ')) pltdev = ' '
       call gettxt('plot_file',  pltfil)
       call gettxt('plot_xlabel', xlabel)
       call gettxt('plot_ylabel', ylabel)
       call gettxt('plot_title', ptitle)
       xkey_x0 = real(getsca('&plot_key_x',0))
       xkey_y0 = real(getsca('&plot_key_y0',0))
       xkey_yd = real(getsca('&plot_key_dy',0))
cc       xkey_y1 = getsca('plot_key_y1',0)
cc       call gettxt('plot_style', tmpsty)
       tmpsty = undef
       tmpcol = undef
       call gettxt('group', name1)
       newdev = .false.
c
       call bkeys(str, mkeys, keys, values, nkeys)
c
       do 50 i = 1, nkeys
          if (keys(i).eq.'reset') then
             call iff_plot_init(1)
             return
          elseif (keys(i).eq.'erase') then
             values(i) = '1'
          elseif ((keys(i).eq.'new').or.
     $            (keys(i).eq.'clear_text')) then
             nplabs = 0
             do 30 j = 1, mplabs
                pltlab(j) = undef
                pltkey(j) = ''
                xplabs(j) = 0
                yplabs(j) = 0
 30          continue
          endif
 50    continue
       jc = 0
c
c set default keywords for "undefined" sets
c ie: the first two unkeyed values will take keys 'x', 'y'
       idfkey    = 1
       ndfkey    = 2
       defkey(1) = '__x'
       defkey(2) = '__y'
c
       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if ((values(i).eq.undef).and.
     $            (idfkey.le.ndfkey).and.(i.le.5)) then
             values(i) = keys(i)
             keys(i)   = defkey(idfkey)
             idfkey    = idfkey + 1
          end if
          if ((keys(i).eq.'prefix').or.(keys(i).eq.'group')) then
             name1 = values(i)
             call smcase(name1,'a')
          elseif (keys(i).eq.'new')   then
             call pgeras
             nplot   = 0
             nplabs  = 0
             npmark  = 0
             nparro  = 0
             do 70 j = 1, 4
                xlim(j)  = 0
                tlim(j)  = 0
                limits(j) = .false.
 70          continue
          elseif (keys(i).eq.'x') then
             xfunc = values(i)
             xinp  = .true.
             call lower(xfunc)
          elseif (keys(i).eq.'y') then
             yfunc = values(i)
             yinp  = .true.
             call lower(yfunc)
          elseif (keys(i).eq.'__x') then
             xfunc = values(i)
             call lower(xfunc)
          elseif (keys(i).eq.'__y') then
             yfunc = values(i)
             call lower(yfunc)
          elseif (keys(i).eq.'dy') then
             dyfunc = values(i)
             call lower(dyfunc)
             err_by  = .true.
          elseif (keys(i).eq.'dx') then
             dxfunc = values(i)
             call lower(dxfunc)
             err_bx  = .true.
          elseif (keys(i).eq.'file') then
             pltfil = values(i)
          elseif (keys(i).eq.'device') then
             pltdev = values(i)
             newdev = .true.
          elseif (keys(i).eq.'erase') then
             call pgeras
             nplot  = 0
             nplabs = 0
             doplot = .false.
          elseif ((keys(i).eq.'xlabel').or.(keys(i).eq.'xtitle')) then
             xlabel = values(i)
          elseif ((keys(i).eq.'ylabel').or.(keys(i).eq.'ytitle')) then
             ylabel = values(i)
          elseif (keys(i).eq.'title') then
             ptitle = values(i)
          elseif (keys(i).eq.'text') then
             plabel = values(i)
             call sclean(plabel)
          elseif (keys(i).eq.'text_x') then
             ier = iff_eval_re( values(i), xpltmp)
          elseif (keys(i).eq.'text_y') then
             ier = iff_eval_re( values(i), ypltmp)
          elseif (keys(i).eq.'grid') then
             igrid = 1
          elseif (keys(i).eq.'nogrid') then
             igrid = 0
          elseif (keys(i).eq.'gridcolor') then
             pltcol(mcolgr) = values(i)
          elseif ((keys(i).eq.'bg').or.(keys(i).eq.'background')) then
             pltcol(mcolbg) = values(i)
          elseif ((keys(i).eq.'fg').or.(keys(i).eq.'foreground')) then
             pltcol(mcolfg) = values(i)
          elseif (keys(i).eq.'color') then
             tmpcol = values(i)
          elseif (keys(i).eq.'style') then
             tmpsty = values(i)
          elseif (keys(i).eq.'width') then
             ier = iff_eval_in(values(i), ilnwid)
          elseif (keys(i).eq.'key') then
             pkey = values(i)
          elseif (keys(i).eq.'linewidth') then
             ier = iff_eval_in(values(i), ilnwid)
          elseif (keys(i).eq.'outlinestyle') then
             ier = iff_eval_in(values(i), ilnsty)
          elseif (keys(i).eq.'charfont') then
             ier = iff_eval_in(values(i), ichrfn)
c
c axisiz   size of characters in axis labels and title
c txtsiz   size of characters in text strings and legend
c mkrsiz   size of points and other plotting markers
c
          elseif (keys(i).eq.'charsize') then
             ier    = iff_eval_re(values(i), axisiz)
             mkrsiz = axisiz
             txtsiz = axisiz
          elseif (keys(i).eq.'labelsize') then
             ier = iff_eval_re(values(i), axisiz)
          elseif (keys(i).eq.'textsize') then
             ier = iff_eval_re(values(i), txtsiz)
          elseif (keys(i).eq.'markersize') then
             ier = iff_eval_re(values(i), mkrsiz)
c
          elseif (keys(i).eq.'key_x') then
             ier = iff_eval_re(values(i), xkey_x0)
             call setsca('&plot_key_x', xkey_x0*1.d0)
          elseif (keys(i).eq.'key_y0') then
             ier = iff_eval_re(values(i), xkey_y0)
             call setsca('&plot_key_y0',xkey_y0*1.d0)
          elseif (keys(i).eq.'key_dy') then
             ier = iff_eval_re(values(i), xkey_yd)
             call setsca('&plot_key_dy', xkey_yd*1.d0)

c
          elseif (keys(i).eq.'xmin') then
             ier = iff_eval_re(values(i), xlim(1))
             limits(1)  = .true.
          elseif (keys(i).eq.'xmax') then
             ier = iff_eval_re(values(i), xlim(2))
             limits(2)  = .true.
          elseif (keys(i).eq.'ymin') then
             ier = iff_eval_re(values(i), xlim(3))
             limits(3)  = .true.
          elseif (keys(i).eq.'ymax') then
             ier = iff_eval_re(values(i), xlim(4))
             limits(4)  = .true.
          elseif ((keys(i).eq.'new').or.(keys(i).eq.'reset')) then
             continue
          elseif (k.ge.1) then
             messg = keys(i)(1:k)//'" will be ignored'
             call warn(1,' ** plot -- unknown keyword "'//messg)
          end if
 99       continue
 100   continue
c
c
       if (.not.(xinp.or.yinp)) then
          if ((nplot.eq.0) .and.
     $         (xfunc.eq.undef).and.(yfunc.eq.undef)) then
             call warn(2,' ** plot warning: nothing to plot')
             return
          else if (yfunc .eq. undef) then
             yfunc = xfunc
             xfunc = '_implicit_'
          endif
       else if (.not.xinp.and.yinp) then
             xfunc = '_implicit_'
       endif
cc       print*, ' plot: ', xinp, yinp, xfunc, yfunc

c if we have x- and y-array expressions, so let's generate these arrays
       if (doplot)  then
          if (nplot.ge.maxplt) then
             call warn(1,' ** plot -- '//
     $            ' at maximum number of plot traces')
          end if
          if ((xfunc.ne.undef).and.(yfunc.ne.undef)) then
             nplot = min(nplot + 1, maxplt)
cc             print*, ' increment nplot to  ', nplot
c
             isarr = iff_eval(yfunc,name1, xxarr,ny1)
             if (isarr.gt.0) then
                pllims(3,nplot) = xxarr(1)
                pllims(4,nplot) = xxarr(1)
                do 220 i = 1, ny1
                   plot_y(i,nplot) = xxarr(i)
                   pllims(3,nplot) = min(pllims(3,nplot),
     $                  real(xxarr(i)))
                   pllims(4,nplot) = max(pllims(4,nplot),
     $                  real(xxarr(i)))
 220            continue
                errby(nplot) = .false.
                if (err_by) then
                   errby(nplot) = .true.
                   isarr =iff_eval(dyfunc,name1,dxarr,ny1)
                   do 230 i = 1, ny1
                      plot_dy1(i,nplot) = xxarr(i) - dxarr(i)
                      plot_dy2(i,nplot) = xxarr(i) + dxarr(i)
                      pllims(3,nplot) = min(pllims(3,nplot),
     $                     real(xxarr(i)-dxarr(i)))
                      pllims(4,nplot) = max(pllims(4,nplot),
     $                     real(xxarr(i)+dxarr(i)))
 230               continue
                endif
             endif

c  if yfunc given, but xfunc is not, use previous x array or, 
c  if first trace to be plotted, simple index for x
             if (xfunc .eq. '_implicit_') then
                if (nplot .eq. 1) then
                   nx1 = ny1
                   do 250 i = 1, nx1
                      xxarr(i) = i
 250               continue
                else
                   nx1 = nplpts(nplot-1)
                   do 260 i = 1, nx1
                      xxarr(i) = plot_x(i,nplot-1)
 260               continue
                endif
                isarr = 999
             else
                isarr =iff_eval(xfunc,name1,xxarr,nx1)
             endif
             if (isarr.gt.0) then
                pllims(1,nplot) = xxarr(1)
                pllims(2,nplot) = xxarr(1)
                do 270 i = 1, nx1
                   plot_x(i,nplot) = xxarr(i)
                   pllims(1,nplot) = min(pllims(1,nplot),
     $                  real(xxarr(i)))
                   pllims(2,nplot) = max(pllims(2,nplot),
     $                  real(xxarr(i)))
 270            continue
                errbx(nplot) = .false.
                if (err_bx) then
                   isarr =iff_eval(dxfunc,name1,dxarr,nx1)
                   errbx(nplot) = .true.
                   do 280 i = 1, nx1
                      plot_dx1(i,nplot) = xxarr(i) - dxarr(i)
                      plot_dx2(i,nplot) = xxarr(i) + dxarr(i)
                      pllims(1,nplot) = min(pllims(1,nplot),
     $                     real(xxarr(i)-dxarr(i)))
                      pllims(2,nplot) = max(pllims(2,nplot),
     $                     real(xxarr(i)+dxarr(i)))
 280               continue
                endif
             endif
          endif
       endif
c
       if (nplot.eq.0) return
cc          call echo(' ** plot warning: nothing to plot')

       nplpts(nplot) = min(nx1, ny1)
       if (nplpts(nplot).le.1) then
          if (nx1.le.1) then
             call warn(2,' ** plot error: no data in x array ')
          elseif (ny1.le.1) then
             call warn(2,' ** plot error: no data in y array ')
          end if
          return
       end if
c
c determine plot limits:  my goodness, this is ugly.
c
c   first, find the real limits of the data from the arrays
       tlim(1) = pllims(1,1)
       tlim(2) = pllims(2,1)
       tlim(3) = pllims(3,1)
       tlim(4) = pllims(4,1)
       do 440 i = 2, nplot
          tlim(1) = min(tlim(1), pllims(1,i))
          tlim(2) = max(tlim(2), pllims(2,i))
          tlim(3) = min(tlim(3), pllims(3,i))
          tlim(4) = max(tlim(4), pllims(4,i))
 440   continue
       if (abs(tlim(2)-tlim(1)).le.prange_min)
     $      tlim(2) = tlim(2) + prange_min

       if (abs(tlim(4)-tlim(3)).le.prange_min)
     $      tlim(4) = tlim(4) + prange_min
c
c  let the user-selected limits override those we just found
       if (limits(1))  tlim(1) = xlim(1)
       if (limits(2))  tlim(2) = xlim(2)
c
c   and if limits(1) or limits(2)  (xmin or xmax) were set, use the
c   truncated data range to select a new set of tlim(3) & tlim(4)!
       if (limits(1).or.limits(2)) then
cc          print*, 'limits:  ', limits(1), limits(2)
cc          print*, tlim(1), tlim(2),  tlim(3), tlim(4), nplpts(1)
          ix1  = nofxsp(tlim(1), plot_x(1,1), nplpts(1))
          ix2  = nofxsp(tlim(2), plot_x(1,1), nplpts(1))
          ix1  = max(1, ix1)
          ix2  = min(nplpts(1), ix2)
cc          print*, ' ix1, ix2, nplot = ', ix1, ix2, nplot
          tlim(3) = plot_y(ix1, 1)
          tlim(4) = plot_y(ix1, 1)
          do 490 i  = 1, nplot
             ix1  = max(1,        nofxsp(tlim(1),plot_x(1,i),nplpts(i)))
             ix2  = min(nplpts(i),nofxsp(tlim(2),plot_x(1,i),nplpts(i)))
             do 470 j = ix1, ix2
                tlim(3) = min(tlim(3), plot_y(j,i))
                tlim(4) = max(tlim(4), plot_y(j,i))
 470         continue
 490      continue
       end if
c or, if they were  expliticly set, use those
       if (limits(3))  tlim(3) = xlim(3)
       if (limits(4))  tlim(4) = xlim(4)
c
c OK, now we really, really have the plot limits in tlim.  whew.
c
c plot text
       if (plabel .ne. undef) then
          nplabs = nplabs + 1
          pltlab(nplabs) = plabel
          xplabs(nplabs) = xpltmp
          yplabs(nplabs) = ypltmp
       end if
c
c plot key (legend)
       call sclean(pkey)
       ilkey = istrln(pkey)
       if ( (pkey .ne. undef).and.(pkey.ne.'').and.
     $      (pkey.ne.' ').and.(ilkey.ge.1)) then
          pltkey(nplot) = pkey
c       else
c          pltkey(nplot) = ''
       end if

c
c
c now execute the plot
c||||||
c|| pgplot-specific code
c||||||
c work over device and output file
       ipd   = max(1, istrln(pltdev))
       plotf = pltdev(1:ipd)
       if ( (pltdev(2:4).eq.'gif').or.
     $      (pltdev(2:4).eq.'png').or.
     $      (pltdev(2:4).eq.'cps').or.
     $      (pltdev(2:4).eq.'vps').or.
     $      (pltdev(2:5).eq.'vcps').or.
     $      (pltdev(2:5).eq.'tpng').or.
     $      (pltdev(2:3).eq.'ps')) then
          if (pltfil.eq.' ')  pltfil = 'ifeffit.'//pltdev(2:)
          itmp  = max(1, istrln(pltfil))
          plotf = pltfil(1:itmp)//pltdev(1:ipd)
       end if
c
c if opening a new device:
c       print*, 'iff_plot: nplot, curdev', nplot, icurdev
c       print*, 'iff_plot: newdev: ', newdev, plotf(:16),
c     $      ":", pltdev(:10),":"
c       do i = 1, mpdevs
c          print*, i, ipgdev(i), pgdevs(i)
c       enddo
cc       print*, 'PLOT newdev? ', newdev
       if (newdev) then
          ipgdev(2) = pgopen(plotf)
          icurdev   = ipgdev(2)
          call pgslct(ipgdev(2))
       else
          pltdev = ' '
          plotf  = ' '
       endif
       if (icurdev .le. 0) then
          call warn(2,' ** plot error: cannot open device:'//plotf)
          return
       end if
          
cc       print*, 'PLOT: pgpage'
       call pgpage
       call pgbbuf
       call pgask(.false.)
cc       print*, 'PLOT: raise plot'
       call iff_plotraise(1)
c set style for the most recent plot
       if (doplot) then
          if (tmpsty.ne.undef) then
             call  set_plsty(tmpsty, isty(nplot), pltsty(nplot))
          endif
c set color for the most recent plot
c icol selects the default colors for different devices
          do 520 j = 0, mctabl
             if (pltcol(j).ne.undef) call setcol(j,pltcol(j))
 520      continue

          if (tmpcol.ne.undef)  then
             call setcol(nplot, tmpcol)
             pltcol(nplot) = tmpcol
             icol(nplot)   = nplot
          endif
cc          print*,  'nplot, icol = ', nplot, icol(nplot), tmpcol
       end if
cc       if (iprint .ge. 18) then
cc          print*, 'nplot, isty, icol= ',nplot,isty(nplot),icol(nplot)
cc       end if
c "erase" the screen to fill with background color.
       call pgeras
c fill the viewport in a different color.
cc       print*, ' mcolbg = ',mcolbg
       call pgsci(mcolbg)
c set char height, width, and font
       call pgsls(ilnsty)
       call pgsch(axisiz)
       call pgscf(ichrfn)
       call pgslw(ilnwid)
       call pgvstd
       if (.not.(limits(1).or.limits(2))) then
          call pgrnge(tlim(1), tlim(2),tlo,thi)
          tlim(1) = tlo
          tlim(2) = thi
       end if
       if (.not.(limits(3).or.limits(4))) then
          call pgrnge(tlim(3), tlim(4),tlo,thi)
          tlim(3) = tlo
          tlim(4) = thi
       end if
cc       call pgswin(tlim(1), tlim(2), tlim(3), tlim(4))
c
c fill the viewport in a different color.
cc       print*, ' mcolbg = ',mcolbg
cc       call pgsci(mcolbg)
       call pgswin(tlim(1), tlim(2), tlim(3), tlim(4))

c make a colored grid
       if (igrid.eq.1)  then
          xboxs = 'g'
          call pgsci(mcolgr)
          call pgbox(xboxs, 0.0, 0, xboxs, 0.0, 0)
       end if
c
c make the main box and labels in color 1
       call pgsci(mcolfg)
       xboxs = 'bcnst1'
       yboxs = 'bcnst1'
       call pgbox(xboxs, 0.0, 0, yboxs, 0.0, 0)
       call pgmtxt('l', 3.0, 0.5, 0.5, ylabel)
       call pgmtxt('b', 3.0, 0.5, 0.5, xlabel)
       call pgmtxt('t', 0.5, 0.5, 0.5, ptitle)
c
c plot the graph.
       call pgslw(ilnwid)
       ikey = -1
       do 700 i = 1, nplot
          show_key = .false.
          mpts = nplpts(i)
          call pgsci(icol(i))
          call pgsch(mkrsiz)
          idum = isty(i)
cc
cc legend setting, with a few parameters still hardwired in!
          if (istrln(pltkey(i)).ge.1) then
             show_key = .true.
             ikey     = ikey+1
             xxkey(1) = tlim(1)+ abs(tlim(2) - tlim(1))*(xkey_x0-.05)
             xxkey(2) = tlim(1)+ abs(tlim(2) - tlim(1))*(xkey_x0-.02)
             xnkey    = tlim(1)+ abs(tlim(2) - tlim(1))*(xkey_x0)
             xkey_yy = xkey_y0- ikey*xkey_yd

             yykey(1) = tlim(3)+ abs(tlim(4) - tlim(3))*(xkey_yy)
             yykey(2) = yykey(1)
             ynkey    = tlim(3)+ abs(tlim(4) - tlim(3))*(xkey_yy-0.02)
            
          endif
c  plot according to current style
c    lines -- solid, dashed, dotted.
          if ((idum.ge.1) .and. (idum.le.4)) then
             call pgsls(isty(i))
             call pgline(mpts,plot_x(1,i), plot_y(1, i))
             if (show_key) call pgline(2,xxkey, yykey)
c
c    line + points: solid line  and a point of some style
          elseif (idum.ge.5) then
             itmp = 1
             call pgsls(itmp)
             call pgline(mpts,plot_x(1,i), plot_y(1,i))
             if (show_key) call pgline(2,xxkey, yykey)
             itmp = idum - 5
             call pgpt(mpts,plot_x(1,i), plot_y(1,i),itmp)
             if (show_key) call pgpt(2,xxkey, yykey,itmp)
c    points only:
          elseif (idum.le.0) then
             itmp = -idum
             call pgpt(mpts,plot_x(1,i), plot_y(1,i), itmp)
             if (show_key) call pgpt(2,xxkey, yykey,itmp)
          end if
c
c error bars:
          if (errby(i)) then
             call pgerry(mpts,plot_x(1,i),
     $            plot_dy1(1,i),plot_dy2(1,i),3.0)
          endif
          if (errbx(i)) then
             call pgerrx(mpts,plot_dx1(1,i),
     $            plot_dx2(1,i),plot_y(1,i),3.0)
          endif
c legend key: same size as axis labels
          if (istrln(pltkey(i)).ge.1) then
             call pgsch(txtsiz)
             call pgtext(xnkey,ynkey,pltkey(i))
          endif
 700   continue
c text labels
       call pgsci(mcolfg)
       call pgsch(txtsiz)
       do 800 i = 1, nplabs
          if (pltlab(i).ne. undef) then
             call pgtext(xplabs(i), yplabs(i), pltlab(i) )
          end if
 800   continue
c markers
       call pgsch(mkrsiz)
       do 820 i = 1, npmark
          if (imarker(i) .ge. -1000) then
             call pgsci(mrkcol(i))
             call pgpt1(xmarks(i), ymarks(i), imarker(i))
          endif
 820   continue
       call pgsci(mcolfg)
c arrow
       do 830 i = 1, nparro
          xxkey(1) = xarros(i,1)
          yykey(1) = xarros(i,2)
          xxkey(2) = xarros(i,3)
          yykey(2) = xarros(i,4)
          call pgsls(1)
          call pgsci(int(xarros(i,8)))
          if (iarrow(i) .eq. -1) then
             call pgline(2,xxkey,yykey)
          else
             call pgsch(xarros(i,7))
             call pgsah(iarrow(i),xarros(i,5),xarros(i,6))
             call pgarro(xxkey(1),yykey(1),xxkey(2),yykey(2))
          end if
 830   continue

c
c  empty plot buffer
       call pgebuf
c  overwrite plot variables
       call settxt('group',       name1)
       call settxt('plot_device', pltdev)
       call settxt('plot_file',   pltfil)
       call settxt('plot_xlabel', xlabel)
       call settxt('plot_ylabel', ylabel)
       call settxt('plot_title',  ptitle)
cc
cc if this was a hardcopy-plot, close device and hand control
cc back to default plot device
       if (icurdev .gt. 1) then
          ix1 = istrln(pltfil)
          call echo(' wrote plot file ' // pltfil(:ix1))
          call pgclos
          call pgslct(ipgdev(1))
          pgdevs(2) = ' '
          ipgdev(2) = -1
          icurdev   =  1
          plotf     =  ' '
       end if
c end subroutine iff_plot
       return
       end

