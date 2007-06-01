       integer function ifeffit(string)
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
c purpose:    main ifeffit function.
c             parse and send string as a command to one of 
c             the ifeffit subroutines
c arguments:  string    command line for ifeffit            [in]
c returns:    0   success 
c            -2   in the middle of defining a macro
c            -1   get another line
c             1   normal exit
c            >1   failure of some sort
c
c notes:      see documentation for further discussion
c
c copyright (c) 1998  matt newville
c
c   Leaving there and proceeding for three days towards the east, you
c   reach Diomira, a city with sixty silver domes, bronze statues of
c   all the gods, streets paved with lead, a crystal theater, a golden
c   cock that crows each morning on a tower.  All these beauties will
c   already be familiar to the visitor, who has seen them also in
c   other cities.  But the special quality of this city for the man
c   who arrives there on a September evening, when the days are
c   growing shorter and the multicolored lamps are lighted all at once
c   at the doors of the food stalls and from a terrace a woman's voice
c   cries ooh!, is that he feels envy toward those who now believe
c   they have once before lived an evening identical to this and who
c   think they were happy, that time.
c
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'echo.h'
       save
       double precision getsca
       character*2048 line, argu, string*(*), val, key*128
       character*128  build
       integer        ilen, ik1, ik2, init, istrln, jmac, iff_load
       integer        iffcmd_exec
       logical        wrthis, isamac
       external istrln, getsca, isamac, iff_load, iffcmd_exec
       data init /1/
c
c init>0 means to initialize 
c  initial string may be used to alter behavior:
c   &quiet :   suppress all screen echo's, even startup message
c 
       if (init .gt. 0) then
          init  = 0
          ilen  = 0
          jmac  = 0
          argu  = string
          call iff_init
          call echo_init
          call iff_plot_init(-1)
          call iff_config
          call triml(argu)
          if (argu(1:6) .eq. '&quiet') then
             call setsca( '&screen_echo',  zero)
             i_echo = 0
          endif
c
       end if
       line   = string
       argu   = ' '
       wrthis = .true.
cc       print*, 'str = ', line(1:40)
c
c  get next command line
c    getline converts blank lines and comment lines to "def", 
c    so we check here that there really is an argument to 
c    avoid repeated trivial processing
       ifeffit = -1
       call iff_getline(line, key, val, argu, ilen)
       ik1 = istrln(key)
       ik2 = istrln(argu)
cc       print*, ' =>', key(1:ik1), ' <', argu(1:ik2),'> ', ilen
       if ((key.eq.'def').and.(argu.eq.' ').and.
     $      (ilen.ge.0)) ilen = 0
c
c  if we're in the middle of a macro definition ...
       if (mac_define(1).ge.1) then
          call iff_macro_def(line)
cc          print*, ' setting ifeffit from ', ifeffit, ' to -2'
          ifeffit = -2
          call triml(line)
          call lower(line)
          if (line(1:9).eq.'end macro') then
             ifeffit = 0
             mac_define(1) = -1
          endif
          return
       endif
c
c  if ilen < 0, the line will be continued to the next line: 
cc       print*, ' ilen = ', ilen
       if (ilen.eq.0) ifeffit = 0
       if (ilen.le.0) return
       ifeffit = 3
       iprint= int(getsca('&print_level',0))
c
c=== execute ifeffit commands based on keyword ===c
c

c
c first check for meta-commands that cannot occur within macros
c

cc       print*, 'IFF key = ', key(1:30), jmac, isamac(key,jmac)
c  execute a macro
       if (isamac(key,jmac)) then
cc          print*, ' jmac= ', isamac(key,jmac), jmac
          call iff_macro_do(jmac, argu, iprint,wrthis)
          wrthis = .false.
          key  = ' '
          argu = ' '

c  define a macro  (more precisly, __begin__ a macro definition)
       else
          ifeffit = iffcmd_exec(key,argu,wrthis)
       endif
c===  write history ===
c  and  information about argument line
       if (histry.and.wrthis)  then
          write(val, '(1x,4a)') key(1:ik1),' (',argu(1:ik2),')'
          if (iprint.ge.19)        call echo(val)
          ilen = istrln(val)          
          write(iohist, '(a)') val(1:ilen)
       endif
c  clean up used line -- we're done
       line = '                                                   '//
     $      '                                                     '//
     $      '                                                     '//
     $      '                                                     '//
     $      '                                                     '
     
       call sclean(line)
cc       print*, 'ifeffit ret ', ifeffit
       if ((ifeffit.ne.-2).and.(ifeffit.ne.1)) ifeffit = 0

c end function ifeffit
       return
       end


       integer function iffcmd_exec(key,argu,wrthis)
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'echo.h'
       save

       character*2048 argu, key*128
       logical  wrthis

       integer  ilen, ik1, ik2, init, istrln, iff_load
       external istrln, iff_load

ccc       print*, ' iffcmd ', key(1:20)
       iffcmd_exec = 0
       if (key .eq. 'macro') then
          call iff_macro_def(argu)
          iffcmd_exec = -2
          wrthis = .false.
c  quit is a special case
       elseif ((key .eq. 'quit').or.(key .eq. 'exit')) then
          call iff_done
          iffcmd_exec = 1
          return
c  load: read a set of command lines from a file
       elseif (key .eq. 'load')  then
          iffcmd_exec = iff_load(argu)
          if (iffcmd_exec.eq.1) return
          wrthis = .false.             
       elseif (key .eq. 'history')  then
          call iff_history(argu)
          wrthis = .false.

c  data i/o commands
       elseif (key .eq. '__test') then
          call iff_test(argu)
       elseif (key .eq. 'read_data') then
          call iff_rddata(argu)
       elseif (key .eq. 'write_data') then
          call iff_wrdata(argu)
       elseif (key .eq. 'save') then
          call iff_save(argu)
       elseif (key .eq. 'restore') then
          call iff_restore(argu) 
c  set/show arrays and scalars
       elseif (key .eq. 'log') then
          call iff_log(argu)
       elseif (key .eq. 'show') then
          call iff_show(argu,0)
       elseif (key .eq. 'print') then
          call iff_print(argu)
       elseif (key .eq. 'echo') then
          call iff_echo(argu,0)
       elseif (key .eq. 'pause') then
          call iff_echo(argu,1)
       elseif ((key .eq. 'def').or.(key .eq. 'define').or.
     $         (key .eq. 'set').or.(key .eq. 'guess')) then
          call iff_set(key,argu,wrthis)
       elseif (key .eq. 'unguess') then
          call iff_unguess(argu)
       elseif (key .eq. 'erase') then
          call iff_erase(argu)
       elseif ((key .eq. 'rename')) then
          call iff_rename(argu)
c  data manipulation commands
       elseif ((key .eq. 'pre_edge')) then
          call iff_pre_edge(argu)
       elseif ((key .eq. 'bkg_cl')) then
          call iff_bkg_cl(argu)
       elseif ((key .eq. 'spline').or.(key .eq. 'autobk')) then
          call iff_spline(argu)
       elseif (key .eq. 'random') then
          call iff_random(argu)
       elseif ((key .eq. 'fft').or.(key .eq. 'fftf').or.
     $         (key .eq. 'bft').or.(key .eq. 'fftr')) then
          call iff_fft(key,argu)
       elseif (key .eq. 'window')  then
          call iff_window(argu)
       elseif (key .eq. 'path')  then
          call iff_path(argu)
       elseif (key .eq. 'get_path')  then
          call iff_getpath(argu)
       elseif (key .eq. 'ff2chi')  then
          call iff_ff2chi(argu)
       elseif (key .eq. 'feffit')  then
          call iff_feffit(argu)
       elseif (key .eq. 'chi_noise')  then
          call iff_chieps(argu)
       elseif (key .eq. 'f1f2')  then
          call iff_f1f2(argu)
       elseif (key .eq. 'diffkk')  then
          call iff_diffkk(argu)
       elseif (key .eq. 'minimize')  then
          call iff_minimize(argu)
       elseif (key .eq. 'correl')  then
          call iff_correl(argu)
       elseif (key .eq. 'error')  then
          call iff_uncert(argu)
       elseif (key .eq. 'sync')  then
          call iff_sync
       elseif (key .eq. 'reset') then
          call iff_init
          call echo_init
          call iff_plot_init(1)
          call iff_config
c
c  plotting commands
       elseif ((key .eq. 'plot').or.
     $         (key .eq. 'overplot').or.(key.eq.'replot'))  then
          call iff_plot(argu)
       elseif (key .eq. 'newplot') then
          call iff_plot('new=1, '//argu)
       elseif (key .eq. 'plot_marker') then
          call iff_plotmarker(argu)
       elseif (key .eq. 'plot_text') then
          call iff_plottext(argu)
       elseif (key .eq. 'plot_arrow') then
          call iff_plotarrow(argu)
       elseif (key .eq. 'zoom') then
          call iff_zoom(argu)
       elseif (key .eq. 'cursor') then
          call iff_cursor(argu)
       elseif (key .eq. 'color') then
          call iff_color(argu)
       elseif (key .eq. 'linestyle') then
          call iff_pstyle(argu)
c  comment is used to write argu to history file only as a comment
       elseif (key .eq. 'comment') then
          if (histry) then
             call rmquot(argu)
             ilen = istrln(argu)
             write(iohist, '(1x,2a)') '# '//argu(1:ilen)
          end if
          wrthis = .false.
       else
          ilen = istrln(key)
          call warn(1,'unknown command: '//key(1:ilen))
       endif
       return 
       end
