       subroutine iff_ff2chi(str)
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
c given a list of paths, create a chi(k) 
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'fefdat.h'
       include 'feffit.h'
       include 'pthpar.h'
       save
       
       character*(*) str
       character*256  name1, namex, list*1024
       integer  idata, irec, istrln, k, j, i, ier, illen
       integer  nxpath,  jk, iupath(mpaths), u2ipth
       integer  nkpts, nkmin, nkmax, nlqpts, iup
       integer  ipath_tmp(max_pathindex)
       double precision  xkmin, xkmax, s02, sigma2
       double precision  aix(maxpts),  arx(maxpts)
       logical  do_pha, do_mag, do_re, isnum
       integer  iff_eval, iff_eval_dp, iff_eval_in
       external iff_eval, iff_eval_dp, iff_eval_in

       external istrln, u2ipth, isnum

       do 10 i = 1, mpthpr
          param(i) = zero
 10    continue

c  interpret any and all keyword/value pairs for setting options
       call bkeys(str, mkeys, keys, values, nkeys)
       idata  =  1
       irec   =  0
       nxpath =  0
       illen  =  1
       list   =  ' '
       name1  =  'feff'
       xkmin  = zero
       xkmax  = 20 * one
       do_re  = .false.
       do_mag = .false.
       do_pha = .false.
       do 100 i = 1, nkeys
          k = istrln( keys(i))
          if ((keys(i).eq.'prefix').or.(keys(i).eq.'group')) then 
             name1 = values(i)
             call smcase(name1,'a')
          elseif ((keys(i).eq.'kmax')) then
             ier = iff_eval_dp(values(i), xkmax)
          elseif ((keys(i).eq.'kmin')) then
             ier = iff_eval_dp(values(i), xkmin)
          elseif ((keys(i).eq.'s02')) then
             ier = iff_eval_dp(values(i), s02)
          elseif ((keys(i).eq.'sigma2')) then
             ier = iff_eval_dp(values(i), sigma2)
          elseif ((keys(i).eq.'data_set')) then
             ier = iff_eval_in(values(i), idata)
          elseif ((keys(i).eq.'do_real')) then
             call str2lg(values(i), do_re, ier)
          elseif ((keys(i).eq.'do_mag')) then
             call str2lg(values(i), do_mag, ier)
          elseif ((keys(i).eq.'no_mag')) then
             call str2lg(values(i), do_mag, ier)
             do_mag = .not. do_mag
          elseif ((keys(i).eq.'do_phase')) then
             call str2lg(values(i), do_pha, ier)
          elseif ((keys(i).eq.'no_phase')) then
             call str2lg(values(i), do_pha, ier)
             do_pha = .not. do_pha
          elseif ((keys(i).eq.'do_all')) then
             call str2lg(values(i), do_re, ier)
             do_mag = do_re
             do_pha = do_re
          elseif (values(i).eq.undef) then
             call str2il(keys(i), max_pathindex, nxpath,ipath_tmp,ier)
             if (ier.eq.0) then 
                jk    = istrln(keys(i))
                list  = list(1:illen)//keys(i)(1:jk)//','
                illen = illen+jk+1
             else
                call warn(2,' *** ff2chi: error generating path list')
                call warn(2, keys(i)(1:k))
             end if
          else
             call warn(1,' *** ff2chi: unknown key: '//keys(i)(1:k))
          end if
 100   continue 
c
c convert list of indices to l_paths
       call str2il(list(1:illen), max_pathindex,nxpath,ipath_tmp,ier)
       do i = 1, mpaths
          iupath(i) = 0
       enddo
       iup= 0

       do 120 i = 1, nxpath
          if (u2ipth(ipath_tmp(i)).ge.1) then
             iup=iup+1
             iupath(iup) = ipath_tmp(i)
          endif
 120   continue 
       nxpath= iup

c read the needed feff arrays
       call fefinp
c
c synchronize the math expressions
       call iff_sync
c
c calculate number of k-points

       nkmin = (xkmin / qgrid)
       nkmax = (xkmax / qgrid)
       nkpts = nkmax - nkmin + 1
       nlqpts= nkmax + 10

cc       print*, 'ff2chi: nkmin,nkmax, nkpts, nlqpts'
cc       print*, nkmin,nkmax, nkpts, nlqpts
c
c  do sum over paths (note : iupath is held in common /fitint/)

       call sum_paths(idata, iupath, nxpath, nlqpts, arx, aix)
c       
c done with sum over paths
c save arrays to Program Variables
c--k
       do 200 i = 1, nlqpts
          tmparr(i) = qgrid * (i + nkmin - 1)
 200   continue 
       call set_array('k',   name1, tmparr(1+nkmin), nkpts, 1)
       call set_array('chi', name1, aix(1+nkmin),  nkpts, 1)
c
c--chi_real
       if (do_re) then
          call set_array('chi_real', name1, arx(1+nkmin), nkpts, 1)
       end if
c--chi_mag
       if (do_mag) then
          do 240 i = 1, nlqpts
             tmparr(i) = sqrt ( arx(i)**2 + aix(i)**2 )
 240      continue 
         call set_array('chi_mag', name1, tmparr(1+nkmin), nkpts, 1)
       end if
c--chi_phase
       if (do_pha) then
          do 280 i = 1, nlqpts
             tmparr(i) =  atan2( aix(i), arx(i))
             if (i.gt.1)  call pijump( tmparr(i), tmparr(i-1))
 280      continue 
          call set_array('chi_phase', name1, tmparr(1+nkmin),nkpts,1)
       end if
c
c done
       return
       end
