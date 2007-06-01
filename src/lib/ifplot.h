c{ifplot.h -*-fortran-*-
c common blocks for plotting in ifeffit
c parameters:
       integer   mcolbg, mcolfg, mcolgr, mctabl,  mpdevs
       integer   mparro, mpmark, mplabs
       parameter (mctabl= maxplt+8, mpdevs = 2)
       parameter (mplabs= 32, mpmark = 32, mparro = 32)
       parameter (mcolbg= mctabl-2, mcolfg= mctabl-1, mcolgr= mctabl)
       integer   jplin, nplot, igrcol, ibgcol, ifgcol
       integer   npmark, nparro, nplabs
       integer   ilnwid, ilnsty, ichrfn, igrid
       integer   inplot, ipgdev(mpdevs), icurdev
       integer   ipgwin(4), icurwin, imarker(mpmark)
       integer   iarrow(mparro), mrkcol(mpmark)
       integer   icol(maxplt), isty(maxplt), nplpts(maxplt)
       character*32 pltcol(0:mctabl), pltsty(maxplt), pgdevs(mpdevs)
       character*32 pltkey(maxplt)
       character*64 pltlab(mplabs)
       logical   limits(4), errbx(maxplt), errby(maxplt)
       real      plot_x(maxpts,maxplt),  plot_y(maxpts,maxplt)
       real      plot_dx1(maxpts,maxplt), plot_dy1(maxpts,maxplt)
       real      plot_dx2(maxpts,maxplt), plot_dy2(maxpts,maxplt)
       real      pllims(4,maxplt),  xlim(4), tlim(4)
       real      axisiz, txtsiz, mkrsiz
       real      xplabs(mplabs), yplabs(mplabs)
       real      xmarks(mpmark), ymarks(mpmark)
       real      xarros(mparro,8)
cc  common /chrplt/ pltdev, pltfil,xlabel, ylabel, ptitle, plabel
       common /plot/ limits, jplin, nplot, nplabs, igrcol, ibgcol,
     $      ifgcol, ilnwid, ilnsty, ichrfn, igrid, inplot,
     $      icol, isty, nplpts, pllims, plot_x, plot_y, xplabs, 
     $      yplabs, xlim, tlim, axisiz, txtsiz,  
     $      xmarks, ymarks, xarros, iarrow, imarker, mrkcol,
     $      mkrsiz, ipgdev, icurdev , npmark, nparro

       common /plattr/ pltcol, pltsty, pltlab,pgdevs
c ifplot.h}







