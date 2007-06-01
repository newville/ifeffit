       subroutine autint
c
c  initialize arrays in common blocks for autobk
c-----------------------------------------------------------------------
       include 'autobk.h'
       integer   i
c common block char
c note :  versn initialized in main program (easier maintainence)
       xmuf    = ' '
       skeyxm  = ' '
       theorf  = ' '
       skeyth  = ' '
       chif    = ' '
       commnt  = ' '
       asccmt  = '# '
       if (vaxflg) asccmt = '##'
       frminp  = ' '
       frmout  = ' '
       do 40 i = 1, maxdoc
          xmudoc(i) = ' '
          thedoc(i) = ' '
 40    continue
c
c common block data
       nxmu    = maxpts
       imucol  = 2
       mdocxx  = 0
       iodot   = 1
       ntheor  = maxpts
       nsplin  = 0
       nkeyxm  = 0
       nkeyth  = 0
       do 100 i = 1, maxpts
          energy(i) = zero
          xmudat(i) = zero
          spline(i) = zero
          chiq(i)   = zero
          thiq(i)   = zero
          windo(i)   = zero
 100   continue     
       do 120 i = 1, mtknot
          eknot(i) = zero
 120   continue     
c
c common block edge
       eefind  = .true.
       stfind  = .true.
       ee      = zero
       predg1  =  -50.
       predg2  = -200.
       slopre  = zero
       bpre    = zero
       enor1   = 100. 
       enor2   = 300. 
       step    = zero
       nnorm   = 3
       nterp   = 1
c
c common block fft
       winstr  = 'hanning'
       iwindo  = 0
       nqpts   = 0
       nrpts   = 0
       qweigh  = one
       qmin    = zero
       qmax    = zero
       windo1  = zero
       windo2  = zero
       rbkg    = one
       r1st    = rbkg + 2 * one
c
c common block ino
c note :  vaxflg initialized in main program (easier maintainence)
       preout  = .false.
       nrmout  = .true.
       eshout  = .false.
       bkgxmu  = .true.
       bkgchi  = .false.
       bkgrsp  = .false.
       thechi  = .false.
       thersp  = .false.
       chirsp  = .false.
       gvknot  = .false.
       iprint  = 0
c
c common block fit
       theory  = .false.
       thefix  = .false.
       eevary  = .true.
       funnrm  = .false.
       final   = .false.
       nrbkg   = 0
       nr1st   = 0
       emin    = zero
       emax    = zero
       e0shft  = zero
       theamp  = one
       thessq  = one
       usrtol  = one
       spstep  = one
c
       return
c  end subroutine autint
       end
