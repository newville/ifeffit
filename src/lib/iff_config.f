       subroutine iff_config
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
       integer i, j, ilen, istrln, nwords
       character*128 build, envvar*32, pgdev*16, words(2)
       character*256 envval, sysdir
       character*512 inifile, sfile
       double precision x
       logical exist
       external istrln
c
       envval = ''
       sysdir = ''
       sfile  = ''
       build  = ''

       include 'com.h'
       include 'sys.h'
c
c----------------------------------------------------------------
c system build information, and setting of 'environmental variables'
c
c pre-load a few general floating point scalars
       x = maxpts * 1.d0
       call setsca( '&maxpts',   x)
       x = maxheap_array * 1.d0
       call setsca( '&heap_free',  x)

       call setsca( '&max_scalars',   maxsca*1.d0)
       call setsca( '&max_arrays',    maxarr*1.d0)
       call setsca( '&max_strings',   maxtxt*1.d0)
       call setsca( '&max_paths',     mpaths*1.d0)
       call setsca( '&max_varys',     mvarys*1.d0)
       call setsca( '&max_data_sets', mdata*1.d0)
       call setsca( '&max_output_cols', max_outarr*1.d0)
       call setsca( '&max_iteration', 250.d0)

       call setsca( '&n_scalars',     zero)
       call setsca( '&n_arrays',      zero)
       call setsca( '&n_strings ',    zero)
       call setsca( '&n_guess',       zero)
       call setsca( '&n_scalars_set', zero)
       call setsca( '&n_scalars_def', zero)
       call setsca( '&n_arrays_set',  zero)
       call setsca( '&n_arrays_def',  zero)
       call setsca( '&print_level',   zero)
       call setsca( '&pause_ignore',  zero)
       call setsca( '&echo_lines',    zero)
       call setsca( '&screen_echo',   zero)
       call setsca( '&sync_level',    4.d0)
       call setsca( '&status', zero )

       call setsca( 'pi', pi)
       call setsca( 'etok', etok)
       call setsca( 'correl_min',  0.05d0)


       call settxt('plot_xlabel', ' ')
       call settxt('plot_ylabel', ' ')
       call settxt('plot_label', ' ')
       call settxt('plot_title', ' ')
c
c default file and array names
       call settxt( 'group',       'my')
       call settxt( 'commentchar', '#')

       call settxt( '&build ', build)
c
c Environmental Variable IFEFFIT_DIR -> Program Variable &install_dir 
c which can overwrite location of data and startup files
       envvar = 'IFEFFIT_DIR'
       ilen   = istrln(envvar)
       call getenv(envvar(1:ilen), envval)
       call triml(envval)
       call sclean(envval)
       ilen   = istrln(envval)
       if (ilen.gt.1)  sysdir = envval
       call settxt('&install_dir', sysdir)

       envvar = 'PGPLOT_DEV'
       ilen   = istrln(envvar)
       call getenv(envvar(1:ilen), envval)
       call triml(envval)
       call sclean(envval)
       ilen   = istrln(envval)
       if (ilen.gt.1)  pgdev = envval
       call settxt('plot_device', pgdev)
c
c load system and personal startup files, if found
       nwords = 2
       words(1)=''
       words(2)=''
       call bwords(inifile, nwords, words)

       if ((sysdir.ne.'').and.(istrln(words(1)).ge.2)) then
          i = istrln(sysdir)
          j = istrln(words(1))
          sfile  = sysdir(1:i)//'/'//words(1)(:j)
          inquire(file = sfile, exist=exist)
          if (exist)  then
             i = istrln(sfile)
             sfile = '"'//sfile(1:i)//'"'
             call iff_load(sfile)
          endif
       endif
c
c load user startup file, if found
       envvar = 'HOME'
       ilen = istrln(envvar)
       call getenv(envvar(1:ilen), sysdir)
       if (sysdir.eq.'') then
          call gettxt('&install_dir', sysdir)
       endif

       if ((sysdir.ne.'').and.(istrln(words(2)).ge.2)) then
          i = istrln(sysdir)
          j = istrln(words(2))
          sfile = sysdir(1:i)//'/'//words(2)(:j)
          inquire(file = sfile, exist=exist)
          if (exist)  then
             i = istrln(sfile)
             sfile = '"'//sfile(1:i)//'"'
             call iff_load(sfile)
          endif
       endif
       return
       end
