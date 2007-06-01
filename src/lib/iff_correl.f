       subroutine iff_correl(str)
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
c
c purpose: turn two-variable correlation to Program Variable
c 
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'feffit.h'
       save
       character*(*)  str
       character*128    xvar, yvar, out, defkey(3)*64
       double precision xn, xcmin, getsca
       logical   lprint, lxall, lyall, lsave
       integer   ier, i, k, istrln, ndfkey, jx, jy, lx, ly
       external  istrln, getsca
c
c get default values for pre-edge parameters from current scalar values
       lprint = .false.
       lsave  = .true.
       call iff_sync
       call bkeys(str, mkeys, keys, values, nkeys)
       xvar = ' '
       yvar = ' '
       out  = undef
       xcmin= getsca('correl_min',1)
       
c set default keywords for "undefined" sets
c ie: the first two unkeyed values will take keys 'x', 'y'
       ndfkey    = 2
       defkey(1) = 'x'
       defkey(2) = 'y'
       do 100 i = 1, nkeys
cc          print*,i, keys(i)
          k = istrln( keys(i))
          if ((values(i).eq.undef).and.(i.le.ndfkey)) then
             values(i) = keys(i)
             keys(i)   = defkey(i)
          end if
          if (keys(i).eq.'x') then
             xvar = values(i)
             call lower(xvar)
          elseif (keys(i).eq.'y') then
             yvar = values(i)
             call lower(yvar)
          elseif (keys(i).eq.'out') then
             out = values(i)
             call lower(out)
          elseif (keys(i).eq.'min') then
             call str2dp(values(i), xcmin, ier)
          elseif (keys(i).eq.'print') then
             call str2lg( values(i), lprint, ier)
          elseif (keys(i).eq.'save') then
             call str2lg( values(i), lsave, ier)
          elseif (keys(i).eq.'no_save') then
             call str2lg( values(i), lsave, ier)
             lsave = .not.lsave
          else
             messg = keys(i)(1:k)//' " will be ignored'
             call warn(1,' *** correl: unknown keyword " '//messg)
          end if
 100   continue
c
c get/resolve array names

       lx  = istrln(xvar)
       ly  = istrln(yvar)
       lxall = xvar.eq.'@all'
       lyall = yvar.eq.'@all'
cc       print*, 'correl ', xvar(1:lx), ' and ', yvar(1:ly)
cc       print*, ' save/print ',lsave,lprint
       jx = 0
       jy = 0
       
       do 150 i = 1, nvarys
          if (scanam(i).eq.xvar(1:lx)) jx = i
          if (scanam(i).eq.yvar(1:ly)) jy = i
 150   continue 

       if (nvarys.le.1) then
          call warn(2,' *** correl: too few variables!')
       else
          if (lxall.and.lyall) then
             do 240 jx = 1, nvarys
                xvar = scanam(jx)
                lx   = istrln(xvar)
                do 230 jy = 1, jx-1
                   yvar = scanam(jy)
                   ly   = istrln(yvar)
                   write(out,'(4a)') 'correl_',xvar(1:lx),
     $                  '_', yvar(1:ly)
                   call iff_correl_s(jx,jy,out,xcmin,lprint,lsave)
 230            continue 
 240         continue 
          elseif (lxall) then
             ly  = istrln(yvar)
             do 280 jx = 1, nvarys
                xvar = scanam(jx)
                lx   = istrln(xvar)
                write(out,'(4a)') 'correl_',xvar(1:lx),
     $               '_', yvar(1:ly)
                if (jx.ne.jy)
     $               call iff_correl_s(jx,jy,out,xcmin,lprint,lsave)
 280         continue 
          elseif (lyall) then
             lx  = istrln(xvar)
             do 290 jy = 1, nvarys
                yvar = scanam(jy)
                ly   = istrln(yvar)
                write(out,'(4a)') 'correl_',xvar(1:lx),
     $               '_', yvar(1:ly)
                if (jx.ne.jy)
     $               call iff_correl_s(jx,jy,out,xcmin,lprint,lsave)
 290         continue 
          else
             if ((jx.le.0).or.(jy.le.0)) then
                if (jx.le.0) then
                   messg = xvar(1:lx)//' is not a fitting variable'
                   call warn(1,' *** correl: '//messg)
                elseif (jy.le.0) then
                   messg = yvar(1:ly)//' is not a fitting variable'
                   call warn(1,' *** correl: '//messg)
                endif
             else
                if (out.eq.undef) then
                   lx  = istrln(xvar)
                   ly  = istrln(yvar)
                   write(out,'(4a)') 'correl_',xvar(1:lx),
     $                  '_', yvar(1:ly)
                endif
             endif
             call iff_correl_s(jx,jy,out,xcmin,lprint,lsave)
          endif 
       endif
       return
c end  subroutine iff_correl
       end
       subroutine iff_correl_s(jx,jy,out,xmin,lprint,lsave)

       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'feffit.h'
       save
       character*(*)  out
       double precision xn, xmin
       logical lprint, lsave
       integer jx,jy, il, istrln
       external istrln

       xn = correl(jx,jy)
       if (abs(xn) .gt. abs(xmin)) then
          if (lsave) call setsca(out,xn)        
          if (lprint) then
             il = istrln(out)
             write(messg,99) out(1:il), xn
             call echo(messg)
          endif
       endif
 99    format(2x,a,' =',f12.6)
       return
       end



