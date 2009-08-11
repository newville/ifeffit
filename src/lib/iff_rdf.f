       subroutine iff_rdf(str)
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
c purpose: read data arrays from external file
c
c arguments:
c      str     command line for ifeffit                 [in]
c
c notes:
c   1. currently supports only ascii column files
c   2. supports several file "types" (xmu,chi,etc) used to name
c      the read-in arrays.
c   3. to add: read array names from the label lines:
c           #------
c           #  energy  xmu  bkg 
c
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       save
       integer    mwords, nt_max, maxbuf, jxline
       parameter (mwords = 128, nt_max = 64, maxbuf=2**17)
       integer    mpts, lun, npts, nwords, nwords1, ndata, in_titles
       integer    narrs, ier, iex, i, j, k, idot, ilen, ilpre, in
       character*(*)  str, stat*10, predef*10, comchr*1, cchars*5
       character*128  pre, file, type, form, tmpnam
       character*2048 line, labstr, label*512
       character*32   words(mwords), suffix, defkey(3)*64
       double precision buffer(maxbuf), sindex(maxpts)
       integer    ndfkey, llen, lslen, nlabs, iofarr, istrln, ntitle
       integer    isort
       logical    isvnam,  istitl, iscomm, isdat
       logical    save_titles, do_sort
       parameter (stat = 'old',predef = 'my')
       integer   iff_eval_in, iread
       external  iff_eval_in
       external  istrln, isvnam, iofarr, isdat, iread
c
       pre    = ' '
       file   = ' '
       labstr = ' '
       form   = 'ascii'
       type   = 'label'
       save_titles = .true.
       do_sort= .false.
       isort  = 1
       in_titles = 0
       call gettxt('commentchar', comchr)
       call settxt('column_label', '--undefined--')
       mpts = maxpts
       narrs  = 0
       npts   = 0
       ndata  = 0
       jxline = 0

c  interpret any and all keyword/value pairs for setting options
       call bkeys(str, mkeys, keys, values, nkeys)

c set default keywords for "undefined" sets
c ie: the first two unkeyed values will take keys 'energy', 'xmu'
       ndfkey    = 1
       defkey(1) = 'file'
       do 10 i = 1, nkeys
          if ((values(i).eq.undef).and.(i.eq.ndfkey)) then
             values(i) = keys(i)
             keys(i)   = defkey(i)
          end if
          if ((keys(i).eq.'prefix').or.(keys(i).eq.'group')) then
                pre = values(i)
                call lower(pre)
                if (.not.(isvnam(pre, 1) ))   call fixnam(pre,1)
                ilpre = istrln(pre)
           elseif (keys(i).eq.'file') then 
             file = values(i)
          elseif (keys(i).eq.'type') then 
             type   = values(i)
          elseif (keys(i).eq.'label') then 
             labstr = values(i)
             call lower(labstr)
             type   = 'user'
          elseif (keys(i).eq.'format') then 
             form = values(i)
          elseif (keys(i).eq.'sort') then 
             call str2in(values(i),isort,ier) 
             do_sort = .true.
          elseif (keys(i).eq.'commentchar') then 
             comchr = values(i)(1:1)
          elseif (keys(i).eq.'notitles') then 
             call str2lg(values(i),save_titles,ier) 
             save_titles = .not.save_titles
          elseif (keys(i).eq.'titles') then 
             call str2lg(values(i),save_titles,ier) 
          elseif (keys(i).eq.'title_lines') then 
             call str2in(values(i),in_titles,ier) 
          elseif (keys(i).eq.'narrays') then 
             ier = iff_eval_in(values(i), narrs)
          else
             if (i.eq.1) then
                file = keys(i)
             else
                k = istrln( keys(i))
                messg = keys(i)(1:k)//' " will be ignored'
                call warn(1,
     $               ' *** read_data: unknown keyword " '//messg)
             end if
          end if
 10    continue 
c
c now that the needed program variables are known, the strategy is:
c   1. open file, with error checking
c   2. determine # of arrays to read
c   3. assign names and positions for arrays to be read
c   4. read arrays from file
c   5. assign number of points for each array
c
c  open file
       if (form.ne.'ascii')  then
          messg = 'unsupported file format!!'
          call warn(2,' *** read_data: '//messg)
          return
       end if
       if (file.ne.' ') then
          iex = 0
          ier = 0
          lun = -1
          call openfl(lun, file, stat, iex, ier)
          if ((ier.lt.0).or.(iex.lt.0)) then
             call echo(' *** read_data: error opening file ')
             ilen = istrln(file)
             messg =   ' ***    '//file(1:ilen)
             call warn(3,messg)
             if (lun.gt.0) close(lun)
             return
          end if
c set prefix from file name 
          if (pre.eq.' ') then 
             idot = index(file,'.')
             if (idot.eq.0) then
                pre = file
             else
                pre = file(1:idot-1)
             end if
             if (pre.eq.' ') pre = predef
             call lower(pre)
             if (.not.(isvnam(pre, 1) ))   call fixnam(pre,1)
             ilpre = istrln(pre)
          end if
       else
          call warn(2,' *** read_data: no file name given!!')
          if (lun.gt.0) close (lun)
          return
       end if
c
c read file, ignoring title lines at top of file.
c  -  when we read the first row of numerical data, count the 
c     number of columns and assign names and positions to vectors
       nwords = mwords
       ntitle = 0
       cchars = ';#%'//comchr
       istitl = .true.
       iscomm = .true.
       
c
c
c title lines
 100   continue
       ilen = iread(lun,  line)
       if (ilen .lt. 0) goto 2900
       jxline = jxline + 1
       iscomm = (line(1:1).ne.' ').and.(index(cchars,line(1:1)).ne.0)
       if ((ilen.le.0).or.(line.eq.' ')
     $      .or.(jxline.le.in_titles)) goto 100
c check for label line
       if  (line(3:7).eq.'-----')  istitl = .false.
       if ((line(3:7).eq.'-----').and.(type.eq.'label')) then
          ilen = iread(lun, line)
          if (ilen .lt. 0) goto 700
          jxline = jxline + 1
          if (iscomm) line = line(2:)
          call triml(line)
          call lower(line)
          nwords  = mwords
          call bwords(line, nwords, words)
          llen  = 1
          do 130 i = 1, nwords
             j      = index(words(i),'.')
             suffix = words(i)(j+1:)
             lslen  = istrln(suffix)
             labstr = labstr(1:llen)//blank//suffix(1:lslen)
             llen   = istrln(labstr)
 130      continue 
c nlab:  number of labels read in
          nlabs = nwords
          goto 200
       end if
c
c read titles into temporarily-named arrays (they'll be renamed 
c according to group below)
 145   format ('$',a,'_title_',i2.2)
       if (iscomm) then
          if (save_titles.and.istitl.and.(ntitle.lt.nt_max)) then
             line   = line(2:)
             ntitle = ntitle + 1
             write(tmpstr, 145) pre(1:ilpre), ntitle
             call settxt(tmpstr,   line)
          endif
          goto 100
       elseif (isdat(line)) then
          nwords = mwords
          call bwords(line, nwords, words)
          goto 210
       else
          goto 100
       end if 
c
c
c  read numerical data
 200   continue
       ilen = iread(lun, line)
       if (ilen .lt. 0) goto 700
       jxline = jxline + 1
       iscomm = (line(1:1).ne.' ').and.(index(cchars,line(1:1)).ne.0)
       if (iscomm.or.(ilen.le.0).or.(line.eq.' ')) goto 200
 210   continue
       nwords = mwords
       call bwords(line, nwords, words)
c  here we have the first real row of numerical data. 
c  save number of arrays to use
       npts = npts + 1
       if (npts.eq.1) then
          nwords1 = nwords
          if (narrs.eq.0) narrs = nwords
          if ((type.ne.'label').and.(type.ne.'user')) then
             do 250 in = 1, narrs
                call file_type_names(type,in,suffix)
                lslen  = istrln(suffix)
                labstr = labstr(1:llen)//blank//suffix(1:lslen)
                llen   = istrln(labstr)                
 250         continue
          endif
       endif
       if (nwords.ne.nwords1) then
          write(messg, '(3x,a,i5)') ' *** read_data: inconsistent '//
     $         'number of columns at line  ',  jxline
          call warn(2, messg)
       endif
cc       print*,  jxline, nwords, '   : ', line(1:30)
       do 580 i = 1, nwords
          ndata = ndata + 1
          call str2dp(words(i), buffer(ndata), ier)
          if (ier.ne.0) then
             if (lun.gt.0) close (lun)
             call echo(' *** read_data: non numeric data in file!')
             write(messg, '(3x,a,i5)') ' *** read_data: at line  ',
     $             jxline
             call echo(messg)
             write(messg, '(3x,a)') line(1:ilen)
             call warn(2,messg)
             return
          end if
 580   continue 
       if (j.ge.maxbuf) then
          write(messg, '(3x,a)')
     $         ' *** read_data: file larger than buffer size'
          call warn(2,messg)
          return
       else
          goto 200
       endif
 700   continue 
c
c  done reading numerical data
cc       print*, 'LINE 700 ', nwords, ndata, npts, narrs
       if (narrs.le.0) goto 2900
       npts = ndata / narrs
c
c now we're done reading the data.
c sort data by a specified column
       
       do 1010 i = 1, npts
          sindex(i) = i
 1010  continue 
       if (do_sort) then
          if ((isort.le.0).or.(isort.gt.narrs)) isort = 1
          do 1020 i = 1, npts
             tmparr(i) = buffer(isort + (i-1) * narrs)
 1020     continue 
          call sort2(npts, tmparr,sindex)
       endif


c
c put data into arrays and construct output label line
       nlabs = mwords
       do 1400 i = 1, mwords
          words(i) = ' '
 1400  continue 
       call bwords(labstr, nlabs, words)
       llen  = 1
       label = ' '
       do 1600 i = 1, narrs
          suffix = words(i)
          if (.not.(isvnam(suffix, 1) ))  then
             call fixnam(suffix,2)
             if (suffix .eq. undef) suffix = ' '
          end if
c avoid repeated arrays: since we're sure that the indarrs are different, 
c       and renaming according to them (without using iff_rename), we 
c       have to be on the look-out for this case.
          do 1530 j = 1, i-1
             if (suffix.eq.words(j)) suffix = ' '
 1530     continue 
          if (suffix.eq. ' ') write(suffix,'(i3)') i
          call triml(suffix)
          tmpnam = pre(1:ilpre)//'.'//suffix
          lslen = istrln(suffix)
          label = label(1:llen)//blank//suffix(1:lslen)
          llen  = istrln(label)
          do 1550 j = 1, npts
             tmparr(sindex(j)) = buffer(i +  (j-1) * narrs)
 1550     continue 
          call set_array(suffix, pre, tmparr, npts, 1)
 1600  continue 
c
c finally set all the program variables
       call settxt('group',    pre)
       call settxt('filename', file)
       call settxt('commentchar', comchr)
       call settxt('column_label', label)
c close unit if still opened
 2900  continue 
       if (lun.gt.0) close(lun)
       call iff_sync
       return
c  end subroutine iff_rdf
       end
