       subroutine iff_show(string,islog)
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
c  show values for ifeffit
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'encod.h'
       include 'echo.h'
       include 'feffit.h'
       include 'fefdat.h'
       include 'pthpar.h'
       save
       character*(*)  string, str*256, s*85, class*16
       integer  iw, i, j, k, istrln, ier, ifeffit, inpath, ip, id
       integer  islog, iex, iup
       integer ipths(mpaths), ilen, np, u2ipth, ndx, jshow
       logical isarr, istext, found
       double precision getsca
       external istrln,  ifeffit, u2ipth, getsca
       str = string


       if (str.eq.'@all') then
          str = '@scalars, @variables, @arrays, @strings, '//
     $         '@commands, @macros, @colors, @paths, @memory, @limits'
       endif

       if (int(getsca('&sync_level',0)).ge.2)  call iff_sync
       nkeys = mkeys
       do 5 i = 1, nkeys
          keys(i) = blank
 5     continue 
       call bwords(str, nkeys, keys)
       iw = 0
 200   continue
c          print*, 'show:: ', iw
          iw = iw + 1
          if (iw.gt.nkeys) then
             return
          endif
          k = istrln(keys(iw))
          s = keys(iw)(1:k)
          call lower(s)
          found  = .false.
          isarr  = (index(s,'.').ne.0)
          istext = (s(1:1).eq.'$')
          if (istext) s = s(2:)
          class  = 'scalar'
          if (istext) then
             class  = 'string'
          else if (isarr) then
             class = 'array'
          endif
          if (s.eq.'@colors') then 
             call  iff_color('show')
             found = .true.
          elseif (s.eq.'@linestyles') then 
             call  iff_pstyle('show')
             found = .true.
          elseif (s.eq.'@groups') then 
             found = .true.
             call ishgrp
          elseif (s.eq.'@arrays') then 
             found = .true.
             do 400 i = 1, maxarr-1
                if (arrnam(i).ne.blank) then
                   call isharr(i)
                   if (iprint.ge.16)
     $                  call rpndmp(icdarr(1,i))
                end if 
 400         continue 
          elseif (s.eq.'@scalars') then 
             found = .false.
             do 410 i = 1, maxsca
                if (scanam(i).ne.blank) then
                   call ishsca(scanam(i),scafrm(i), scalar(i))
                   if (iprint.ge.12)
     $                  call rpndmp(icdsca(1,i))
                   found = .true.
                end if
 410         continue 
          elseif (s.eq.'@system') then 
             found = .false.
             do 415 i = 1, maxsca
                if ((scanam(i).ne.blank) .and.
     $               (scanam(i)(1:1).eq.'&')) then
                   call ishsca(scanam(i),scafrm(i), scalar(i))
                   if (iprint.ge.12)
     $                  call rpndmp(icdsca(1,i))
                   found = .true.
                end if
 415         continue 
          elseif (s.eq.'@variables') then 
             found = .true.
             do 420 i = 1, nvarys
                if ((icdsca(1,i).eq.-1).and.(scanam(i).ne.blank))
     $               call ishvar(scanam(i), scalar(i), delta(i) )
 420         continue 
          elseif (s.eq.'@correlations') then 
             call iff_correl('x=@all,y=@all,print=yes,save=no,')
             found = .true.
             s     = blank
             nkeys = -1
          elseif (s.eq.'@strings') then 
             found = .true.
             do 430 i = 1, maxtxt
                if ((txtnam(i).ne.blank).and. (text(i).ne.blank))
     $               call ishtxt(txtnam(i),text(i))
 430         continue 
          elseif (s.eq.'@commands') then 
             found = .true.
             do 440 i = 1, mckeys
                if ( (ckeys(i).ne.undef).and.(ckeys(i).ne.blank))
     $               call ishcom(ckeys(i), chint(i))
 440         continue 
          elseif (s.eq.'@command') then 
             found = .false.
             class = 'command'
             iw  =  iw+1
             s = keys(iw)
             call lower(s)
             do 443 i = 1, mckeys
                if ( (ckeys(i).ne.undef).and.(ckeys(i).ne.blank)
     $               .and.(ckeys(i).eq.s))
     $               call ishcom(ckeys(i), chint(i))
 443         continue 

          elseif (s.eq.'@macros') then 
             found = .true.
             iw  =  iw+1
             s = keys(iw)
             call lower(s)
             jshow = 1
             if (s.eq.'full') jshow = 0
             do 450 i = 1, mckeys
                if ( (macnam(i).ne.undef).and.
     $               (macnam(i).ne.blank)) call ishmac(macnam(i),jshow)
 450         continue 
          elseif (s.eq.'@macro') then 
             found = .false.
             class = 'macro'
             iw  =  iw+1
             s = keys(iw)
             call lower(s)
             do 460 i = 1, macmax
                if ( (macnam(i).ne.undef).and.
     $               (macnam(i).ne.blank  ).and.
     $               (s.eq.macnam(i)    )) then 
                   call ishmac(macnam(i),0)
                   found = .true.
                end if 
 460         continue 
          elseif (s.eq.'@limits') then 
             call ishow_simple( '&maxpts')
             call ishow_simple( '&max_scalars')
             call ishow_simple( '&max_arrays')
             call ishow_simple( '&max_strings')
             call ishow_simple( '&max_paths')
             call ishow_simple( '&max_varys')
             call ishow_simple( '&max_data_sets')
             found = .true.
          elseif (s.eq.'@memory') then 
             call ishow_simple( '&heap_free')
             call ishow_simple( '&n_scalars')
             call ishow_simple( '&n_arrays')
             call ishow_simple( '&n_strings ')
             call ishow_simple( '&n_guess')
             call ishow_simple( '&n_scalars_set')
             call ishow_simple( '&n_scalars_def')
             call ishow_simple( '&n_arrays_set')
             call ishow_simple( '&n_arrays_def')
             found = .true.
          elseif (s.eq.'@args') then 
             found = .true.
             do 470 i = 1, mmcarg
                if (mcargs(nmacro,i).ne.blank) then
                   write(tmpstr,'(1x,a,i1,2a)') '$',i, ' = ',
     $                  mcargs(nmacro,i)
                   call echo(tmpstr)
                endif
 470         continue 
          elseif (s.eq.'@feffpaths') then 
             found = .true.
cc             print*, ' feff files in use '
             do 530 i = 1, mfffil
                if (feffil(i).ne.blank) then
                   k  = istrln(feffil(i))
                   tmpstr = feffil(i)(1:k)
                   if (.not.lffred(i)) then
                      tmpstr = feffil(i)(1:k)//' (not read yet)'
                   endif
                   call echo(tmpstr)
                endif
 530         continue 
          elseif (s.eq.'@paths') then 
             found = .true.
             do 610 ip = 1, mpaths
                call show_path(jdtusr(ip))
 610         continue 
          elseif (s.eq.'@path') then 
             found = .false.
             s    = blank
             ilen = 1
             class  = 'path'
             do 660 i = iw+1, nkeys
                k  = istrln(keys(i))
                s  = s(1:ilen)//keys(i)(1:k)//','
                ilen = ilen + k + 1
 660         continue 
             call str2il(s(1:ilen), mpaths,np,ipths,ier)
             do 680 ip = 1, np
                call show_path(ipths(ip))
                found = .true.
 680         continue 
             iw = nkeys
          elseif (s.eq.'@group') then 
             found = .false.
             class  = 'group'
             iw    = iw+1
             s     = keys(iw)
             do 710 i = 1, maxarr-1
                j = index(arrnam(i),'.')
                if ((arrnam(i).ne.blank) .and.
     $               (j.gt.0).and. (arrnam(i)(1:j-1).eq.s)) then
                   call isharr(i)
                   if (iprint.ge.12)
     $                  call rpndmp(icdarr(1,i))
                   found = .true.
                end if 
 710         continue             
          elseif (istext) then
             found = .false.
             do 760 i = 1, maxtxt
                if (s.eq.txtnam(i)) then
                   call ishtxt(txtnam(i),text(i))
                   found = .true.
                   go to 200
                end if
 760         continue 
          elseif (isarr) then
             found = .false.
             do 820 i = 1, maxarr-1
                if (s.eq.arrnam(i)) then
                   call isharr(i)
                   if (iprint.ge.12)
     $                  call rpndmp(icdarr(1,i))
                   found = .true.
                   go to 200
                end if
 820         continue 
          else
c check (in this order): commands, macros, scalars
             found = .false.
             do 940 i = 1, mckeys
                if (s.eq.ckeys(i)) then
                   call ishcom(ckeys(i), chint(i))
                   found = .true.
                   go to 200
                end if 
 940         continue 
             do 950 i = 1, macmax
                if (s.eq.macnam(i)) then 
                   call ishmac(macnam(i),0)
                   found = .true.
                   go to 200
                end if 
 950         continue 
             do 970 i = 1, maxsca
                if (s.eq.scanam(i)) then
                   call ishsca(scanam(i),scafrm(i), scalar(i))
                   found = .true.
                   if (iprint.ge.12)
     $                  call rpndmp(icdsca(1,i))
                   go to 200
                end if
 970         continue 
          end if
          if (.not.found) then
             call undels(s)
             ier = max(6,istrln(s))
             if (istext)  then
                s = '$'//s(1:ier)
                ier = ier + 1
             endif
             iex = max(7,istrln(class))
             call warn(1, blank//class(1:iex)//blank//
     $            s(:ier)//'  not found')
          end if
          go to 200
       end
c

       subroutine isharr(i)
c
c echo information about an array
c
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       character*256 s, outm, tmp*384
       integer npts, k, kf, istrln, i
       
       external istrln
       s  = arrnam(i)
       if (s.eq.blank) return
       if ( s.eq.undef_array ) then
cc          print*, 'WEIRD:: array ', i, ' is erased, not reclaimed.'
          return
       endif
 10    format(2a,i6,a,g11.4,a,g11.4,a)
 11    format(2a,i6,a,g11.4,a,g11.4,2a)

       k   = max(14, istrln(s))
       write(tmp,10) s(1:k), ' =',  narray(i), ' pts  [',
     $      arrmin(i), ':', arrmax(i), ']' 

       kf  = istrln(arrfrm(i))
       if ((arrfrm(i).ne.'').and.(arrfrm(i).ne.undef)
     $      .and.(kf.ge.1))  then
          write(tmp,11) s(1:k), ' =',  narray(i), ' pts  [',
     $         arrmin(i), ':', arrmax(i), '] := ' , arrfrm(i)(1:kf)
       endif
 	 outm = tmp
       call echo(outm)
c
c       if (iprint.ge.5) then
c          print*, '    array # ', i, ' points to ', nparr(i)
c       endif
c
       return
       end
c
       subroutine ishsca(s,f,x)
       character*(*) s, f, f1*256, messg*256
       double precision   x, small, xmin, zero
       logical  ax
       parameter (small = 1.d-8, xmin = 12.d0, zero=0.d0)
       integer k, kf, ltot, ilx, istrln
       external istrln
       k  = max(14, istrln(s))
       f1 = f
       call triml(f1)
       kf = istrln(f1)
       if (kf.ge.1)   f1 = ' := '//f1(1:kf)
       kf = istrln(f1)
	 ltot = k + kf
	 if (ltot.ge.230) kf = 230 - k
	 messg  = ' '
       if ((x.eq.zero).or.
     $      (abs(log(abs(x + small))).le.xmin)) then 
          write(messg,11)  s(1:k), ' = ', x, f1(1:kf)
       else
	    
          write(messg,12)  s(1:k), ' = ', x, f1(1:kf)
       end if
       call echo(messg)
 11    format (2a,f17.9,a)
 12    format (2a,g17.9,a)
       return
       end

       subroutine ishvar(s,x,dx)
       character*(*) s, messg*256
       double precision x, dx,  small, xmin
       logical  ax, adx
       parameter (small = 1.d-8, xmin = 12.d0)
       integer k, istrln
       external istrln
       k = max(14, istrln(s))
       ax  = abs(log( abs( x + small ))).le.xmin
       adx = abs(log( abs(dx + small ))).le.xmin
       if (ax.and.adx) then
          write(messg,11)  s(1:k), ' = ', x, ' +/- ', dx
       elseif(ax .and. (.not.adx)) then
          write(messg,12)  s(1:k), ' = ', x, ' +/- ', dx
       elseif ((.not.ax) .and. adx) then
          write(messg,13)  s(1:k), ' = ', x, ' +/- ', dx
       else
          write(messg,14)  s(1:k), ' = ', x, ' +/- ', dx
       endif
 11    format (2a,f15.8,a,f15.8)
 12    format (2a,f15.8,a,g15.8)
 13    format (2a,g15.8,a,f15.8)
 14    format (2a,g15.8,a,g15.8)
       call echo(messg)
       return
       end


       subroutine ishow_simple(s)
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       character*(*) s
       integer i, ilen, istrln
       ilen = istrln(s)
       do 10 i = 1, maxsca
          if (scanam(i).eq.s(1:ilen)) then
             call ishsca(scanam(i),scafrm(i), scalar(i))
          end if
 10    continue 
       return
       end

       subroutine ishtxt(s,t)
       character*(*) s, t , messg*256
       integer k, j, istrln
       external istrln
       k = min(max(13, istrln(s)),256)
       j = min(max(2, istrln(t)), 252-k)
       write(messg,11)  s(1:k), ' = ', t(1:j)
       call echo(messg)
 11    format('$',3a)
       return
       end
c
       subroutine ishcom(s,t)
       character*(*) s, t , messg*256
       integer    k, j, istrln
       external istrln
       k = min(max(14, istrln(s)), 256)
       j = min(max(2, istrln(t)), 253-k)
       write(messg,11)  s(1:k), ': ', t(1:j)
       call echo(messg)
 11    format (3a)
       return
       end
c
       subroutine ishmac(str,idonly)
c
c  show contents of a macro
c  idonly = 1 to only show name, arg list, description
c
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       save

       character*(*) str 
       integer i, k, kk, j, istrln, idonly
       external istrln
       i = 0
 10    continue 
       i = i + 1
       if (i.gt.macmax) then
          tmpstr = str
          k      = istrln( tmpstr )
          call warn(1, ' macro '// tmpstr(1:k) //' not found')
          return
       endif
       if ((macnam(i).eq.undef).or.(macnam(i).eq.blank)) go to 10
       if (macnam(i).ne.str) go to 10
c  know we know that it's macro "i" we want
       tmpstr = macnam(i)
       k      = istrln( tmpstr )
       do 60 j = 1, mmcarg
          kk = istrln(mcargd(i,j))
          if (kk.ge.1) then
             if (j.gt.1) then 
                tmpstr = tmpstr(1:k)//', "'// mcargd(i,j)(1:kk)// '"'
             else
                tmpstr = tmpstr(1:k)//'  "'// mcargd(i,j)(1:kk)// '"'
             end if
          end if
          k = istrln(tmpstr)
 60    continue 
       call triml(tmpstr)
       k = istrln(tmpstr)
       call echo( ' macro '// tmpstr(1:k) )
       if ((mcdesc(i).ne.blank).and.(mcdesc(i).ne.undef)) then
          tmpstr = mcdesc(i)
          call triml(tmpstr)
          k = istrln(tmpstr)
          call echo( '   "'// tmpstr(1:k)//'"' )
       endif
c
c idonly = 1 if we only wanted name, arg list, description
       if (idonly.le.0) then
          imac = imacro(i)
 150      continue 
          if ((imac.gt.0).and.(imac.le.mcline)) then
             k = istrln( macstr(imac) )
             call echo( '     '// macstr(imac)(1:k)  )
             imac = imcptr(imac)
             go to 150
          end if
          call echo( ' end macro')
       endif
cc
cc  full dump
cc       if (iprint.eq.20) then 
cc          print*, 'MACROS: '
c          do i = 1, macmax
c             if (macnam(i) .ne.blank) then
c                print*, '  mac: ', macnam(i), imacro(i)
c             endif
c          enddo
c          do i = 1, mcline
c             if (macstr(i) .ne.undef) then
c                print*, ' ', i, imcptr(i), macstr(i)(1:70)
c             endif
c          enddo
cc       endif
cc
       return
       end
c
       subroutine ishgrp
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       character*256 s, sgrps(maxarr)
       integer is, i, j, ii, k, istrln
       external istrln
       save
       is    = 0
       do 80 i = 1, maxarr-1
          j = index(arrnam(i),'.')
          if (j.gt.0) s = arrnam(i)(1:j-1)
          if (is.gt.0) then 
             do 40 ii = 1, is
                if (s.eq. sgrps(ii)) goto 65
 40          continue 
          end if
          is = is + 1
          sgrps(is) = s
          k = max(1, istrln(s))
          write(messg,'(2x,a)') s(1:k)
          call echo(messg)
 65       continue             
 80    continue             
       return
       end
c       
       subroutine show_path(iup)
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'encod.h'
       include 'fefdat.h'
       include 'feffit.h'
       include 'pthpar.h'
       save
       integer  iup, inpath, u2ipth, jfeff, i, ret
       integer  ntmp, ilen,  istrln, xafs_path
       double precision tmp2(maxpts), getsca, xtmp, pdeg
       character*2 at_symbol
       external    at_symbol
       external istrln, u2ipth, getsca, xafs_path
       
       inpath = u2ipth(iup)
       if (inpath .le. 0) return

       jfeff  = jpthff(inpath)
c read feff files if necessary
       if (.not. lffred(jfeff)) then
          call fefinp
          if (int(getsca('&sync_level',0)).ge.3)  call iff_sync
       endif          

       xtmp = iup * 1.d0
       call setsca('path_index', xtmp)
       call synvar
c
       if (iup .le. 0) return
       ret  = xafs_path(iup,tmparr,tmp2,xtmp)
       if (ret.ne.1) return
cc       print*, '::: iup , jfeff ' , iup, inpath, jfeff, refpth(jfeff)

c
c      path degeneracy
       pdeg = param(jfpdeg)       
       if (icdpar(1,jfpdeg,inpath).eq.0) pdeg = degpth(jfeff)

       write(messg,'(1x,a,i5)') ' PATH ', iup
       call echo(messg)
       ret  = xafs_path(iup,tmparr,tmp2,xtmp)

       ilen = istrln(feffil(jfeff))
       write(messg,'(4x,2a)') 'feff   = ', feffil(jfeff)(1:ilen)
       call echo(messg)
c
       ilen = istrln(fpthid(jfeff))
       write(messg,'(4x,2a)') 'id     = ', fpthid(jfeff)(1:ilen)
       call echo(messg)

       ilen = istrln(pthlab(inpath))
       write(messg,'(4x,2a)') 'label  = ', pthlab(inpath)(1:ilen)
       call echo(messg)


c  evaluate the path parameters
       do 50 i = 1, mpthpr
          tmparr(1) = zero
          if (i.eq.jfps02)  tmparr(1) = one
          ntmp = 0
          if (iprint.ge.12)
     $         call rpndmp(icdpar(1,i,inpath))
          if (icdpar(1, i, inpath).ne.0) then
             call decod(icdpar(1, i, inpath), micode, 
     $            consts, scalar, array, narray, nparr, 
     $            maxsize_array, maxarr,  ntmp, tmparr)
          end if
          param(i) = tmparr(1)
 50    continue 
c note for degeneracy: 
c   the default is from feff.dat file, but it can be 
c   overridden as a path parameter.        
       xtmp = param(jfpdeg)       
cc       print*, ' xtmp ', xtmp
       if (icdpar(1,jfpdeg,inpath).eq.0) xtmp = degpth(jfeff)
       call write_double_param('r     ', 6, refpth(jfeff)+param(jfpdr))
       call write_double_param('degen ', 6, xtmp)
       call write_double_param('s02   ', 6, param(jfps02))
       call write_double_param('e0    ', 6, param(jfpe0))
       call write_double_param('dr    ', 6, param(jfpdr))
       call write_double_param('ss2   ', 6, param(jfpss2))
       if (icdpar(1,jfp3rd,inpath).ne.0) then
          call write_double_param('3rd   ', 6, param(jfp3rd))
       endif
       if (icdpar(1,jfp4th,inpath).ne.0) then
          call write_double_param('4th   ', 6, param(jfp4th))
       endif
       if (icdpar(1,jfpei ,inpath).ne.0) then
          call write_double_param('ei    ', 6, param(jfpei) )
       endif
       if (icdpar(1,jfppha,inpath).ne.0) then
          call write_double_param('dphase', 6, param(jfppha))
       endif
       return
       end
       subroutine write_double_param(s,n,f)
       character*(*) s
       double precision f
       integer  n
       include 'consts.h'
       include 'keywrd.h'
 10    format(4x,a,' =',f12.6)       
       write(messg,10) s(1:n), f 
       call echo(messg)
       return 
       end
