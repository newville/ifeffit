       subroutine iff_save(string)
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
c  save session to a  PAD save file
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'encod.h'
       save
       integer       max_groups, igr
       parameter (max_groups=256)
       integer        i, k, istrln,  ndfkey, ilen, inn, get_array
       integer        npack, iex, ier, lun, isn, isf, iff_eval_in
       integer        j,jl
       character*512  string*(*), file, s2, build
       character*256  groups(max_groups)
       character*10   stat, spad*20, vers, defkey(2)*64
       logical s_str, s_sca, s_arr, s_sys, s_psc, dsave
       double precision getsca
       parameter (stat = 'unknown' , vers = '1.02')
       external istrln, iff_eval_in, get_array, getsca

       if (int(getsca('&sync_level',0)).ge.1)  call iff_sync
       file  = 'ifeffit.sav'
       call  gettxt('&build', build)
       npack = 8
       s_str = .true.
       s_sca = .true.
       s_arr = .true.
       s_psc = .false.
       s_sys = .false.
       igr   = 0
       do 10 i = 1, max_groups
          groups(i) = undef
 10    continue 
c  interpret any and all keyword/value pairs for setting options
       call bkeys(string, mkeys, keys, values, nkeys)
       ndfkey    = 1
       defkey(1) = 'file'
       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if ((values(i).eq.undef).and.(i.le.ndfkey)) then
             values(i) = keys(i)
             keys(i)   = defkey(i)
          end if
          if (keys(i).eq.'file')  then
             file = values(i)
          elseif (keys(i).eq.'group')  then
             igr = igr + 1
             if (igr.le.max_groups) groups(igr) = values(i)
          elseif ((keys(i).eq.'npad').or.(keys(i).eq.'npack')) then
             ier = iff_eval_in(values(i),npack)
          elseif (keys(i).eq.'no_strings')  then
             s_str = .false.
          elseif (keys(i).eq.'with_strings')  then
             s_str = .true.
          elseif (keys(i).eq.'no_scalars')  then
             s_sca = .false.
          elseif (keys(i).eq.'with_scalars')  then
             s_sca = .true.
          elseif (keys(i).eq.'pad_scalars')  then
             s_psc = .true.
          elseif (keys(i).eq.'no_pad_scalars')  then
             s_psc = .false.
          elseif (keys(i).eq.'no_arrays')  then
             s_arr = .false.
          elseif (keys(i).eq.'with_arrays')  then
             s_arr = .true.
          elseif (keys(i).eq.'no_sys')  then
             s_sys = .false.
          elseif (keys(i).eq.'with_sys')  then
             s_sys = .true.
          elseif (keys(i).eq.'arrays_only')  then
             s_sca = .false.
             s_str = .false.
             s_arr = .true.
          elseif (keys(i).eq.'scalars_only')  then
             s_sca = .true.
             s_str = .false.
             s_arr = .false.
          elseif (keys(i).eq.'strings_only')  then
             s_sca = .false.
             s_str = .true.
             s_arr = .false.
          else
             messg = keys(i)(1:k)//' " will be ignored'
             call warn(1,' *** save: unknown keyword " '//messg)
          end if
 100   continue 
       call undels(file)
       iex  = 0
       ier  = 0
       lun  = 3
       call openfl(lun, file, stat, iex, ier)
       if ((ier.lt.0).or.(iex.lt.0)) then
          call warn(2,' *** save: error opening file ')
          return
       end if
       write(lun,'(3a,i3)') '#:IFEFFIT SAVE File: v',vers,
     $      '  npad =',npack
       ilen = istrln(build)
       write(lun,'(2a)') '#:build ', build(1:ilen)
       write(lun,'(a)')  '#:strings'
       if (s_str) then
          do 210 i = 1, maxtxt
             if ((txtnam(i).ne.' ').and. (text(i).ne.' ') .and.
     $            (txtnam(i)(1:1).ne.'&')) then
                isn = max(1,istrln(txtnam(i)))
                isf = max(1,istrln(text(i)))
                write(lun,'(4a)') '#= ',txtnam(i)(1:isn),
     $               '= ',text(i)(1:isf)
             endif
 210      continue 
       end if
c
       write(lun,'(a)') '#:system'
       if (s_sys) then 
          do 220 i = 1, maxsca
             if ((scanam(i).ne.' ').and.(scanam(i)(1:1).eq.'&')) then
                call pad(scalar(i),npack,spad)
                isn = max(1,istrln(scanam(i)))
                s2  = scafrm(i)
                isf = istrln(s2)
                if (icdsca(1,i).eq.-1) then
                   write(lun,'(4a)') '#? ', scanam(i)(1:isn), ' ',
     $                  spad(1:npack)
                elseif ((s2.eq.' ').or.(isf.eq.0)) then
                   write(lun,'(4a)') '#% ', scanam(i)(1:isn), ' ',
     $                  spad(1:npack)
                else
                   write(lun,'(4a)') '#= ', scanam(i)(1:isn), ' = ',
     $                  s2(1:isf)
                end if
             end if
 220      continue 
       end if

       if (s_psc) then
          write(lun,'(a)') '#:scalars (pad)'
       else
          write(lun,'(a)') '#:scalars (nopad)'
       endif
       if (s_sca) then 
          do 230 i = 1, maxsca
             if ((scanam(i).ne.' ').and.(scanam(i)(1:1).ne.'&')) then
                call pad(scalar(i),npack,spad)
                spad(npack+1:)= ' '
                if (.not.s_psc) write(spad, '(g19.13)') scalar(i)
                isn = max(1,istrln(scanam(i)))
                s2  = scafrm(i)
                isf = istrln(s2)
                if (icdsca(1,i).eq.-1) then
                   write(lun,'(4a)') '#? ', scanam(i)(1:isn),' ',spad
                elseif ((s2.eq.' ').or.(isf.eq.0)) then
                   write(lun,'(4a)') '#% ', scanam(i)(1:isn),' ',spad
                else
                   write(lun,'(4a)') '#= ', scanam(i)(1:isn), ' = ',
     $                  s2(1:isf)
                end if
             end if
 230      continue 
       end if
c arrays      
       write(lun,'(a)') '#:arrays'
       if (s_arr) then 
          do 340 i = 1, maxarr-1
             if ((arrnam(i).ne.blank) .and. (arrnam(i).ne.undef)) then
                dsave = .true.
                if (igr.ge.1) then
                   dsave = .false.
                   j     = index(arrnam(i),'.')
                   do 305 k = 1, igr
                      jl = istrln(groups(k))
                      if (arrnam(i)(1:j-1).eq.groups(k)(1:jl)) then
                         dsave = .true.
                         goto 307
                      endif
 305               continue 
 307               continue 
                endif
                if (dsave) then
                   isn = max(1,istrln(arrnam(i)))
                   s2  = arrfrm(i)
                   isf = istrln(arrfrm(i))
                   if ((s2.eq.' ').or.(isf.eq.0)) then
                      inn  = get_array(arrnam(i),'',0,tmparr)
                      write(lun,'(a,i6,2a)') '#% [', inn, '] ',
     $                     arrnam(i)(1:isn)
                      call wrpadd(lun,npack,tmparr,inn)
                   else
                      write(lun,'(a,i6,4a)') '#= [', narray(i), '] ', 
     $                     arrnam(i)(1:isn), ' = ', s2(1:isf)
                   end if
                end if 
             endif
 340      continue 
       end if
       if (lun.gt.0) close(lun)
       return
       end
c
       subroutine iff_restore(string)
c
c  restore session from a  PAD save file
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'encod.h'
       save
       integer   i, j, k, istrln,  ndfkey, ierr, in1, in2
       integer   npack, iex, ier, lun, nwords, ntmp, iread_ky
       integer   iofstr, iofsca, ilen, i_sync
       character*512  str, file, s1, s2, string*(*)
       character*256  defkey(2)*64 
       character*10   stat, pre*2, vers, words(2)*(20)
       double precision unpad, xvers, getsca
       logical      sc_pad
       parameter (stat = 'old')
       external unpad, istrln, iofstr, iofsca, iread_ky, getsca
       file = 'ifeffit.sav'
c  interpret any and all keyword/value pairs for setting options
       call bkeys(string, mkeys, keys, values, nkeys)
cc       print*, ' iff_restore : ', string(1:70)
cc       print*, ' iff_restore : ', file(1:40)
       ndfkey    = 1
       defkey(1) = 'file'
       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if ((values(i).eq.undef).and.(i.le.ndfkey)) then
             values(i) = keys(i)
             keys(i)   = defkey(i)
          end if
          if (keys(i).eq.'file')  then
             file = values(i)
          else
             messg = keys(i)(1:k)//' " will be ignored'
             call warn(1,' *** restore: unknown keyword " '//messg)
          end if
 100   continue 
c 
c note that the sync_level is stored here (otherwise we'd "restore" it!)
       i_sync = int(getsca('&sync_level',0))
       if (i_sync.ge.3)  call iff_sync

       call undels(file)
       iex  = 0
       ier  = 0
       lun  = -1
       call openfl(lun, file, stat, iex, ier)
       if ((ier.lt.0).or.(iex.lt.0).or.(lun.le.0)) then
          ilen  = istrln(file)
          messg = ' *** restore: cannot open file '//file(1:ilen)
          call warn(2,messg)
          if (lun.gt.0) close(lun)
          return
       end if
c top line
       ilen = iread_ky(lun, pre, str)
       vers  = 'null'
       npack = 0
       if ((pre.eq.'#:').and.(str(1:20).eq.'IFEFFIT SAVE File: v')) then
          vers = str(21:25)
          call triml(vers)
          call lower(vers)
          call str2dp(vers,xvers,ier)
          s2   = str(26:)
          call lower(s2)
          nwords = 2
          call bwords(s2,nwords,words)
          if (words(1).eq.'npad') call str2in(words(2),npack,ierr)
       endif
       if ((npack.le.0).or.(npack.ge.16).or.(vers.eq.'null')) goto 1010
c next line must be 'build'
       if (xvers.ge.1.01) then
          ilen = iread_ky(lun, pre, str)
          if ((pre.ne.'#:').or.(str(1:5).ne.'build')) goto 1010
       endif
c next line must be 'strings'
       ilen = iread_ky(lun, pre, str)
       if ((pre.ne.'#:').or.(str(1:3).ne.'str')) goto 1010
c read strings
cc       print*,' strings:'
 150   continue 
         ilen = iread_ky(lun, pre, str)
         if ((pre.eq.'#:').and.(str(1:3).eq.'sys')) goto 200
         if (pre.ne.'#=') goto 1010
         nwords   = 2
         words(1) = ' '
         words(2) = ' '
         call bwords(str,nwords,words)
         i         = iofstr(words(1),1)
         txtnam(i) = words(1)
         call strclp(str, words(1), words(2), text(i))
       go to 150
c scalars
 200   continue 

 250   continue 
       nwords = 2
       words(1) = ' '
       words(2) = ' '
       ilen = iread_ky(lun, pre, str)
cc       print*, ' pre, str = ', pre, ',  ', str(1:30)
       if ((pre.eq.'#:').and.(str(1:3).eq.'sca')) goto 300
       call bwords(str, nwords,words)
       i         = iofsca( words(1), 1)
cc       print*, ' iofsca = ', i
       scanam(i) = words(1)
       scafrm(i) = ' '
       if (pre.eq.'#?') then
          scalar(i)   = unpad(words(2), npack)
          icdsca(1,i) = -1
       elseif (pre.eq.'#%') then
          scalar(i)   = unpad(words(2), npack)
          icdsca(1,i) = i + jscale
       elseif (pre.eq.'#=') then
          call iff_set('def',str,.false.)
       else
          goto 1010
       endif
       goto 250
 300   continue 
       sc_pad = .true.
       if ( (xvers.ge.1.015)  .and.
     $      (index(str(4:),'(nopad)').ne.0) ) then
          sc_pad = .false.
       endif
 350   continue 
       nwords = 2
       words(1) = ' '
       words(2) = ' '
       ilen = iread_ky(lun, pre, str)
       if ((pre.eq.'#:').and.(str(1:3).eq.'arr')) goto 400
       call bwords(str, nwords,words)
       i         = iofsca( words(1), 1)
cc       print*, ' iofsca = ', i
       scanam(i) = words(1)
       scafrm(i) = ' '
       if (pre.eq.'#?') then
          if (sc_pad) then
             scalar(i)   = unpad(words(2), npack)
          else
             call str2dp(words(2), scalar(i), ier)
          endif
          icdsca(1,i) = -1
       elseif (pre.eq.'#%') then
          if (sc_pad) then
             scalar(i)   = unpad(words(2), npack)
          else
             call str2dp(words(2), scalar(i), ier)
          endif
          icdsca(1,i) = i + jscale
       elseif (pre.eq.'#=') then
          call iff_set('def',str,.false.)
       else
          goto 1010
       endif
       goto 350
 400   continue 
c arrays
cc       print*,' arrays:'
 650   continue 
       ilen = iread_ky(lun, pre,str)
cc       print*,' pre, ilen ', pre, ' ', ilen
       if (ilen.lt.0) goto 800
       in1 = index(str,'[')
       in2 = index(str,']')
       call str2in(str(in1+1:in2-1),ntmp, ierr)
       str = str(in2+1:)
       call triml(str)
       if (pre.eq.'#=') then
c define array by formula
          call iff_set('def',str,.false.)
       elseif (pre.eq.'#%') then
c
c read array data
          j  = index(str,'.')
          s1 = str(1:j-1)
          s2 = str(j+1:)
          call triml(s1)
          call triml(s2)
          call rdpadd(lun,npack,tmparr, ntmp)
          call set_array(s2,s1,tmparr,ntmp,1)
       else
          goto 1010
       endif
       go to 650

 800   continue 
cc       print*, ' line 800'
c
c arrays      
       if (lun.gt.0) close(lun)
       if (i_sync.ge.1)  call iff_sync
       return
 1010  continue 
       call warn(2,' *** restore: not a valid save file')
       return
       end


