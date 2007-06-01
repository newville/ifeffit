       subroutine iff_init
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
c initialize ifeffit
       implicit none
       include 'consts.h'
       include 'arrays.h'
       include 'encod.h'
       include 'fefdat.h'
       include 'fft.h'
       include 'keywrd.h'
       include 'pthpar.h'
       include 'spline.h'
       include 'echo.h'
       include 'feffit.h'
       save
       integer i, j, k, istrln
       character*64  pltdev, dstr*128, d1str*16, desc
       integer index,  ii, ndevx, inter,  tlen, dlen
       logical exist
       external istrln
c
c write tab delimited outputs (as opposed to space delimited)
       tabdel = .false.
c consts for encoding/decoding: set the first few anyway!
       do 110 i = 10, mconst
          consts(i) = zero
 110   continue 
       do 120 i = 1, 11
          consts(i) = i - 1 
 120   continue 
c
c scalars
       do 630 i = 1, maxsca
          scanam(i) = blank
          scafrm(i) = blank
          scalar(i) = 0
          do 610 j = 1, 2
             icdsca(j,i) = 0
 610      continue 
 630   continue 
c text strings
       do 640 i = 1, maxtxt
          txtnam(i) = blank
          text(i)   = blank
 640   continue 
c arrays
       do 650 j = 1, 8*maxsize_array
          array(j) = zero
 650   continue 
       do 653 j = 1, maxsize_array
          tmparr(j) = zero
 653   continue 
       
       npnext      = 1
       do 680 i = 1, maxarr
          arrnam(i) = blank
          arrfrm(i) = blank
          narray(i) = 0
          nparr(i)  = -1
c         do 650 j = 1, maxpts
c             array(j,i) = 0
c 650      continue 

          do 660 j = 1, 2
             icdarr(j,i) = 0
 660      continue 

 680   continue 
       arrnam(maxarr) = undef_array
c
c default i/o file unit numbers 
c
c /bkgdat/ spline.h initialization
       nsplin = 0
       nautbk = 0
       nrbkg  = 0
       nxmu   = 0
       nr1st  = 0
       theory = .false.
       eevary = .true.
       thefix = .false.
       final  = .false.
       funnrm = .false.
       de0    = 0
       e0     = 0
       emin   = 0
       emax   = 0
       rbkg   = 0
       thessq = one
       thebkg = one
       step   = 0
       do 2100 i = 1, maxpts
          chie(i)   = zero
          endat(i)  = zero
          xmudat(i) = zero
 2100  continue 
c
c /pthpar/ 
       itfeff = 0
       inpthx = 0
       do 2130 i = 1, mpthpr
          defalt(i) = zero
 2130  continue 
       defalt(jfps02) = one
       do 2150 i = 1, mfffil
          lffred(i)  = .false.
          iffrec(i)  = 0
          iffused(i) = 0
          degpth(i)  = zero
          feffil(i)  = blank
 2150  continue 
c
c /fft/ fft.h initialization
       call cffti(maxfft, wfftc)
       wftset = .true.
c
c      /feffit/
       do 2388 i = 1, 10
          restraint(i,1) = undef
 2388  continue 
       fit_macro = undef
       ifit_mac  = 0
       do 2400 i = 1, mpaths
          iulist(i,1) = 0
 2400  continue 
       do 2450 i = 1, mvarys
           xguess(i) = zero
           xfinal(i) = zero
           delta(i)  = zero
           correl(i,i) = zero
           chisqr = zero
 2450   continue 

c
c
c history file
       iohist = 0
       histry = .false.
c macros
       do 3020 i = 1, mcline
          macstr(i) = undef
          imcptr(i) = 0
 3020  continue
       do 3040 i = 1, macmax
          macnam(i) = blank
          mcdesc(i) = blank
          imacro(i) = 0
 3040  continue
       do 3051 i = 1, mcdeep
          imac_save(i) = -1
 3051  continue 
       ioinp  = 0
       nfiles = 0
       do 3060 i = 1, mfiles
          lfiles(i) = blank
          iunit(i) = 10 + i
 3060  continue 
       do 3090 i = 1, mmcarg
          do 3085 j = 1, mcdeep
             mcargs(j,i)  =  blank
 3085     continue 
          do 3087 j = 1, macmax
             mcargd(j,i)  =  blank
 3087     continue 
 3090  continue 
c
c command keys
       mac_define(1) = 0
       mac_define(2) = 0
       mac_define(3) = 0
       mac_define(4) = 0
       mac_exec      = 0
       nmacro        = 0
       nmac_stop     = 0
       do 8020 i = 1, mckeys
          ckeys(i) = undef
          chint(i) = blank
 8020  continue
c
       call seed_randmt(4359)
c
       def_command = 'def'
       call pgqndt(ndevx)
       dstr = ' '
       k    = 1
       do 800 i = 1, ndevx
          call pgqdt(i,d1str,tlen,desc,dlen,inter)
          call triml(d1str)
          call lower(d1str)
          j    = istrln(d1str)
          dstr = dstr(1:k)//' '//d1str(1:j) 
          k    = istrln(dstr)
 800   continue 
       call triml (dstr)
       call settxt('plot_devices', dstr)       
       call setsca('&plot_key_x',0.90d0)
       call setsca('&plot_key_y0',0.90d0)
       call setsca('&plot_key_dy',0.07d0)
       call fstop_init('ifeffit.err')
       return
       end
