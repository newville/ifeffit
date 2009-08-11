       subroutine iff_wrdata(str)
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
c  write data for ifeffit
c
c fields:
c   file  =  file to write data to
c   pref   =  file prefix to use if not supplied
c   npts  =  max number of points to write out
c   col#n =  full name of array to write out in slot #n in the
c            "pref"."suff" form.
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       save

       integer    maxtit, max_outsca
       parameter (maxtit=64, max_outsca=128)
       integer       mpts, iout(max_outarr), lun, is, ilen, idot, lx
       integer       il, ic, jcol, ncol, ntit, nsca, i, j, k, istrln
       integer       iofarr, ier, iex, nmatch, iff_eval_in, ndfkey
       character*(*) str, stat*10, delim*2, comchr*2
       character*2048 sout
       character*512 file, pref,  type, defkey(2)
       character*256 arrout(max_outarr), scaout(max_outsca)
       character*512 titout(maxtit), strarr(maxtxt), labstr
       character*32  uform, fform
       double precision   getsca, xval
       logical       l_titles, isvnam
       parameter (stat = 'unknown')
       external  iofarr, istrln, iff_eval_in, getsca, isvnam
cc       print*, ' iff_wrdata 0' 
       delim = '  '
       labstr   = undef
       l_titles = .true.
       uform =  'g15.8'
       if (tabdel) delim = char(9)

       call gettxt('group', pref)
       file = ' '
cc       call gettxt('filename', file)
cc       call gettxt('filetype', type)
       call gettxt('commentchar', comchr)
       mpts = maxpts
       ncol = 0
       nsca = 0
       ntit = 0
       do 10 i = 1, max_outarr
          arrout(i) = blank
 10    continue 
       do 11 i = 1, max_outsca
          scaout(i) = blank
 11    continue 
       do 12 i = 1, maxtit
          titout(i) = blank
 12    continue 
c  interpret any and all keyword/value pairs for setting options
       call bkeys(str, mkeys, keys, values, nkeys)
       ndfkey    = 1
       defkey(1) = 'file'

       do 100 i = 1, nkeys
          if (keys(i).eq.'group') then 
             pref = values(i)
             call smcase(pref,'a')
          elseif (keys(i).eq.'file') then 
             file = values(i)
          elseif (keys(i).eq.'commentchar') then 
             comchr = values(i)
          elseif (keys(i).eq.'label') then 
             labstr = values(i)
          elseif (keys(i).eq.'no_headers') then 
             call str2lg(values(i), l_titles, ier)
             l_titles = .not. l_titles
          elseif (keys(i).eq.'l_titles') then 
             call str2lg(values(i), l_titles, ier)
          elseif (keys(i).eq.'npts') then 
             ier = iff_eval_in(values(i), mpts)
          else
c fill lists of strings, scalars, arrays according to name
             if (isvnam(keys(i),0)) then
                ncol = ncol + 1
                if (ncol.le.max_outarr)  arrout(ncol) = keys(i)
cc                print*, 'arr: ', keys(i)(1:20)
             elseif (isvnam(keys(i),1)) then
                nsca = nsca + 1
                if (nsca.le.max_outsca)  scaout(nsca) = keys(i)
             elseif ((isvnam(keys(i),3)) .or.
     $               ((keys(i)(1:1).eq.'$').and.
     $               (index(keys(i),'*').ne.0))) then
                ntit = ntit + 1
                if (ntit.le.maxtit)  titout(ntit) = keys(i)(2:)
cc                print*, 'tit: ', titout(ntit)(1:20)
             else
                k = istrln( keys(i) )
                messg = keys(i)(1:k)//' " will not be written'
                call warn(1,
     $               ' *** write_data: unknown variable " '//messg)
             endif
          end if
 100   continue 
c
c now make sure we have the full name and that this array is defined
c
       iex  = 0
       ier  = 0
       lun  = 3
       call openfl(lun, file, stat, iex, ier)
       if ((ier.lt.0).or.(iex.lt.0)) then
          call warn(3,' *** write_data: error opening file ')
          return
       end if
c determine format string: 
       call triml(uform)
       il = istrln(uform)
       lx = 17
       fform = '(1x,'//uform(1:il)//')'
cc       print*, 'fform = ', fform

c
c get array names (for sure)
       jcol = 0
cc       print*, ' nsca, ntit, ncol = ', nsca, ntit, ncol
       ncol = min(ncol, max_outarr)
       if (ncol.eq.0) then
          call warn(2, ' *** write_data:  no data to write out!')
          return
       end if
       do 180 i = 1, ncol
          call glob(arrout(i),arrnam,maxarr,strarr,maxtxt,nmatch)
          do 160 j = 1, nmatch
             k = iofarr(strarr(j),pref,0,0)
             if (k.le.0) then 
                call echo(' *** write_data: couldn''t find array:')
                call warn(1,'     '//strarr(j))
             else
                jcol = jcol + 1
                if (jcol.le.max_outarr) then
                   iout(jcol) = k
                   mpts = min(mpts, narray(k))
                else
                   call echo(' *** write_data: too many arrays'//
     $                  ' requested for write')
                   write(tmpstr,'(5x,a,i3)') 'current limit is ',
     $                  max_outarr
                   call warn(2,tmpstr)
                   return
                endif
             end if
 160      continue
 180   continue
       ncol = min(jcol, max_outarr)
c
c write title lines
       if (l_titles) then 
          ntit = min(ntit, maxtit)
          ic = istrln(comchr)
          if (ntit.eq.0) then
             write(lun,'(3a)') comchr(1:ic),' ','data from ifeffit'
          else
             do 250 i = 1, ntit
cc                print*, ' TITLE ', i,  titout(i)(1:30)
                call glob(titout(i),txtnam,maxtxt,
     $               strarr,maxtxt,nmatch)
                do 200 j = 1, nmatch
                   call gettxt(strarr(j), tmpstr)
                   call triml(tmpstr)
                   k  = istrln(tmpstr)
                   if (k.gt.0) write(lun,'(3a)')
     $                  comchr(1:ic),' ', tmpstr(1:k)
 200            continue 
 250         continue 
          end if
          nsca = min(nsca, max_outsca)
          do 280 i = 1, nsca
             xval = getsca(scaout(i),0)
             write(tmpstr,fform) xval
             k    = istrln(scaout(i))
             write(lun,'(5a)') comchr(1:ic), ' ', scaout(i)(1:k),
     $            ' = ',tmpstr(1:lx)
cc             print*, 'sca: ', i,  scaout(i)(1:k), '  ', tmpstr(1:lx)
 280      continue 
c
c      write label line:  suffixes only!!!
          write(lun,'(2a)') comchr(1:ic),'------------------------'
          sout = comchr(1:ic)//' '
          ilen = istrln(sout) + 1
          do 350 k = 1, ncol 
             il   = max(8, istrln(arrnam(iout(k))))
             idot = index( arrnam(iout(k)), '.')
             sout = sout(1:ilen)//delim//arrnam(iout(k))(idot+1:il)
             ilen = ilen + il + 2
 350      continue 
          ilen = istrln(sout)
          if (labstr .ne.undef) then
             call triml(labstr)
             ilen = istrln(labstr)
             sout = comchr(1:ic)//'  '//labstr(1:ilen)
             ilen = ilen + ic + 2
          endif
          write(lun,'(a)') sout(1:ilen)
       endif          
       if (jcol.ne.ncol) then
          call warn(2,' *** write_data: wrong number of vectors??')
          return
       end if
c write vectors
       do 500 i = 1, mpts
          sout = ' '
          do 470 k = 1, ncol
cc            write(sout(1-lx+k*lx:k*lx),fform) array(i,iout(k))
              write(sout(1+(k-1)*17:k*17),fform)
     $            array(nparr(iout(k))+i-1)
 470      continue
          call triml(sout)
          is = istrln(sout)
          write(lun,'(2x,a)') sout(1:is)
 500   continue
       if (lun.gt.0) close(lun)
c finally, set program variables
       call settxt('group',    pref)
cc       call settxt('filetype', type)
       call settxt('commentchar', comchr)
c
       return
       end

