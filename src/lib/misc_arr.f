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
       integer function iofarr(name,group,npts,icreat)
c
c  return index of array
c  arguments:
c     name    array name (giving only a suffix is allowed)    [in/out]
c     group   group name (' ' allowed -> default looked up)   [in]
c     npts    size of array (used as input only for creation) [in/out]
c     icreat  integer create flag: 0=no create, 1=create OK   [in]
c  returns:
c     iofarr  index of array
c             < 0  :  not found and couldn't create it
c             = 0  : out of range  (ie, too many names defined)
       implicit none
       include 'consts.h'
       include 'arrays.h'
       save

       character*(*) name, group, pre*256
       integer  ioflist, icreat, jcreat, npts, iof
       double precision xpnext
       external ioflist
c
       pre    = group
       if ((pre.eq.'').or.(pre.eq.' ')) call gettxt('group', pre)
       call prenam(pre,name)
       iof    = maxarr
       jcreat = 0
       if (name.ne.' ') then
          iof = ioflist(name,arrnam,maxarr-1,icreat,jcreat)
          if ((icreat.eq.1).and.(jcreat.eq.0).and.(iof.ge.1)) then
cc             print*, ' iofarr: ', name(1:20), ' :',narray(iof),
cc     $            npts, jcreat
             if ((narray(iof).gt.0).and.(narray(iof).lt.npts)) then
                call erase_array(iof,.true.)
                iof = ioflist(name,arrnam,maxarr-1,icreat,jcreat)
             endif
          end if
          if ((jcreat.eq.1).and.(iof.ge.1)) then
cc             print*, ' iofarr create array: ', iof, npts, npnext
             nparr(iof)  = npnext
             narray(iof) = npts
             npnext      = (1+((nparr(iof) + narray(iof))/2))*2
             npnext      = min(maxheap_array-200, max(1, npnext))
             xpnext      = (maxheap_array - npnext)*1.0
             call setsca('&heap_free', xpnext)
             call fixarr(iof, name, npts, 1)
cc             print*, ' end iofarr= ', iof,nparr(iof), npnext
          endif
       end if
       iofarr = iof
       return
       end

       integer function iofsca(name,icreat)
c
c  return index of scalar
c  arguments:
c     name    scalar name                                   [in]
c     icreat  integer create flag: 0=no create, 1=create OK [in]
c  returns:
c     iofsca  index of scalar
c             < 0  :  not found and couldn't create it
c                     (-iofsca would be next available slot)
c             = 0  : out of range  (ie, too many names defined)
       implicit none
       include 'consts.h'
       include 'arrays.h'
       save
       integer  ioflist, icreat, jcreat
       character*(*) name
       external  ioflist

       jcreat = 0
       iofsca = ioflist(name,scanam,maxsca,icreat,jcreat)
       if (jcreat.eq.1) call setsca(name, zero)
       return
       end

       integer function iofstr(name,icreat)
c
c  return index of string variable
c  arguments:
c     name    string name                                   [in]
c     icreat  integer create flag: 0=no create, 1=create OK [in]
c  returns:
c     iofstr  index of string
c             < 0  :  not found and couldn't create it
c                     (-iofstr would be next available slot)
c             = 0  : out of range  (ie, too many names defined)
       implicit none
       include 'consts.h'
       include 'arrays.h'
       save

       integer  ioflist, icreat, jcreat
       character*(*) name, str*256
       external  ioflist

       jcreat = 0
       str    = name
       if (str(1:1).eq.'$')  str = str(2:)
       iofstr = ioflist(str,txtnam,maxtxt,icreat,jcreat)
       if (jcreat.eq.1) call settxt(str, ' ')

       return
       end
       integer function ioflist(name,carray,mxca,icreat,jcreat)
c
c  return index of scalar string in a list of strings
c  arguments:
c     name    scalar name                                   [in]
c     carray  array of name                                 [in]
c     max     dimension of carray                           [in]
c     icreat  integer create flag: 0=no create, 1=create OK [in]
c     jcreat  integer create flag: 1= creation needed       [out]
c  returns:
c     iostr  index of scalar
c             < 0  :  not found and couldn't create it
c                     (-iofsca would be next available slot)
c             = 0  : out of range  (ie, too many names defined)
       integer  istrln, j, i, icreat, iempty, mxca, jcreat
       character*(*) name, carray(mxca)
       external istrln

       j      = max(1, istrln(name))
       jcreat = 0
       iempty = 0
       i      = 0
 10    continue
       i = i + 1
c if we're out-of-range, return the -1 * index of first empty slot
       if (i.ge.mxca) then
          i = -iempty
c if we find an empty slot and are creating, use this one
       elseif ((carray(i).eq.' ').and.(icreat.eq.1) ) then
          carray(i) = name(1:j)
          jcreat    = 1
c if we find the first empty slot and are *not* creating,
c save this slot and keep going
       elseif ((carray(i).eq.' ').and.(icreat.ne.1)
     $         .and.(iempty.eq.0)) then
          iempty = i
          go to 10
c if the name doesn't match, keep going
       elseif (carray(i).ne.name(1:j)) then
          go to 10
       end if
c if name matched, then we end up here:
       ioflist = i
       return
       end
       logical function isvnam(str,itype)
c
c determine if string can be a valid variable name
c  arguments:
c    str    string to check         [in]
c    itype  type of name to check   [in]
c           -1  array or scalar
c            0  full array name
c            1  scalar / array prefix
c            2  string after $ / array suffix
c            3  string (including $)
c
       character*(*) str
       character bslash, squote, dollar, dot, invalid*32, digits*10
       integer   ilen, i, j1, istrln, itype, idot, jdot, ic1
       parameter (invalid = '!@#$%^*+=-/<>`,;"|()[]{}')
       parameter (digits  = '0123456789', dollar = '$', dot = '.')
       external  istrln
       j1     = 1
       ilen   = istrln(str)
       squote = ''''
       bslash = '\\ '
       isvnam = ((index(str(:ilen),squote) .eq. 0) .and.
     $           (index(str(:ilen),bslash) .eq. 0) )
       if (.not.isvnam) return
       idot   = index(str,dot)
cc       print*, ' -- in isvnam: itype = ', itype, ' : ',str(1:40)
       ic1 = ichar(str(1:1))
c either array or scalar:  . cannot be first or last char
       if (itype.eq.-1) then
          isvnam = (idot .ne. 1) .and. (idot .ne. ilen)
c full array names must have a dot (we'll count them below)
       elseif (itype.eq.0) then
          isvnam = (idot .gt. 1) .and. (idot .lt. ilen)
c scalars/prefixed must not begin with a digit
       elseif (itype.le.1) then
          isvnam = (index(digits, str(1:1)) .eq. 0)
c strings must begin with a dollar sign
       elseif (itype.eq.3) then
          isvnam = (str(1:1) .eq. dollar)
          j1   = 2
       end if
c  make sure there are no invalid characters in the name
       if (isvnam) then
          jdot   = 0
          isvnam = .false.
          do 10 i = j1, ilen
             if (index(invalid,str(i:i)) .ne. 0) return
             if (str(i:i).eq.dot) jdot = jdot + 1
 10       continue
c  array names must have 1 '.' -- rest must have 0 '.'.
          isvnam  = jdot.eq.0
          if (itype.eq.0) then
             isvnam  = jdot.eq.1
          elseif (itype.eq.-1) then
             isvnam  = jdot.le.1
          end if
       end if

       return
       end
       subroutine fixnam(str,itype)
c
c  repair bad variable name
c  arguments:
c    str    string to check         [str]
c    itype  type of name to check   [in]
c           -1  array or scalar
c            0  full array name
c            1  scalar / array prefix
c            2  string after $ / array suffix
c            3  string (including $)
c
       include 'consts.h'
       character*(*) str, s*1, s2*2
       character bslash, squote, dollar, dot, invalid*32, digits*10
       integer   ilen, i, istrln, itype
       parameter (invalid = '!@#$%^*+=-/<>`,;"|()[]{}')
       parameter (digits  = '0123456789', dollar = '$', dot = '.')
       external  istrln
       squote = ''''
       bslash = '\\ '
c check that first character is correct
       ilen= istrln(str)
       if (ilen.le.0) then
          s = '_'
          ilen = 1
       endif
       s   = str(1:1)
       if ( (itype.eq.1).and.(index(digits, s) .ne. 0) ) then
          str  = '_'//str(1:ilen)
          ilen = ilen + 1
       elseif ((itype.eq.3).and.(s.ne.dollar)) then
          str  = '$'//str(1:ilen)
          ilen = ilen + 1
       end if
c check rest of characters
       i = 0
 10    continue
       i = i + 1
       s = str(i:i)
       if( ((s.eq.squote) .or. (s.eq.dot) .or.
     $      (s.eq.bslash) .or. (s.eq.blank) .or.
     $      (index(invalid,s).ne.0) ) ) then
          str(i:i) = '_'
       end if
       if (i.lt.ilen) go to 10

c       ilen= istrln(str)
c       print*, ' ilen ', ilen, str(1:20)
c       do 20 i = 1, ilen-1
c          print*, '... ', i, str(i:i+1)
c          if (str(i:i+1).eq.'__') then
c             str(i:i+1) = '_&'
c          endif
c 20    continue

cc strip off leading '_'
c 20    continue
c       if ((str(1:1).eq. '_').and.(index(digits, str(2:2)).eq.0)) then
c          str = str(2:)
c          go to 20
c       endif
c       ilen = istrln(str)
cc strip off trailing '_'
c 30    continue
c       if (str(ilen:ilen).eq. '_')  then
c          str = str(1:ilen-1)
c          ilen = ilen - 1
c          go to 30
c       endif
c make sure we end with a valid, but clearly bad file name
c       if (str .eq.' ') str = undef

       return
       end
       subroutine fixarr(in, name, npts, iconst)
c
c  set max and min values, name, and npts for an array
c     in the ifeffit array "structure"
c arguments:
c   in      index in array lists  (cannot be zero)   [in]
c   name    name to give to this array (if unnamed)  [in]
c   npts    number of points to add                  [in]
c   iconst  integer flag: 1 for 'constant array',    [in]
c                         0 otherwise
       implicit none
       include 'consts.h'
       include 'arrays.h'
       include 'encod.h'
       save
       integer   npts, in, j, iconst
       character*(*) name, group*64
       double precision  ax
c
       if ((in.gt.0).and.(in.le.maxarr)) then
          narray(in) = npts
          if ((arrnam(in).eq.' ').and.(name.ne.' ')) then
             call gettxt('group', group)
             arrnam(in) = name
             call prenam(group,arrnam(in))
          end if
c  set max and min values
cc        print*, ' fixarr: ', in, nparr(in), npts 
          arrmax(in) =  array(nparr(in))
          arrmin(in) =  array(nparr(in))
          do 20 j = 1, npts
             ax = array(nparr(in)+j-1)
             if (arrmin(in).gt.ax) arrmin(in) = ax
             if (arrmax(in).lt.ax) arrmax(in) = ax
 20       continue
c set icdarr (for 'constant arrays')
          if ((iconst.eq.1).or. (icdarr(1,in) .eq. 0)) then
             icdarr(1,in) = in
             icdarr(2,in) = 0
          end if
       end if
       return
c  end subroutine fixarr
       end
       subroutine file_type_names(type,icol,name)
       character*(*) type, name
       integer  icol
 50    format(i3)
       write(name,50)    icol
       if ((type.eq.'xmu').and.(icol.eq.1))  name = 'energy'
       if ((type.eq.'xmu').and.(icol.eq.2))  name = 'xmu'
       if ((type.eq.'pre-edge').and.(icol.eq.1))  name = 'energy'
       if ((type.eq.'pre-edge').and.(icol.eq.2))  name = 'pre'

       if ((type.eq.'chi').and.(icol.eq.-1)) name = 'win'
       if ((type.eq.'chi').and.(icol.eq.1))  name = 'k'
       if ((type.eq.'chi').and.(icol.eq.2))  name = 'chi'

       if ((type.eq.'rsp').and.(icol.eq.-1)) name = 'rwin'
       if ((type.eq.'rsp').and.(icol.eq.1))  name = 'r'
       if ((type.eq.'rsp').and.(icol.eq.2))  name = 'chir_re'
       if ((type.eq.'rsp').and.(icol.eq.3))  name = 'chir_im'
       if ((type.eq.'rsp').and.(icol.eq.4))  name = 'chir_mag'
       if ((type.eq.'rsp').and.(icol.eq.5))  name = 'chir_pha'

       if ((type.eq.'chi_std').and.(icol.eq.1)) name = 'k_std'
       if ((type.eq.'chi_std').and.(icol.eq.2)) name = 'chi_std'

       if ((type.eq.'qsp').and.(icol.eq.-1)) name = 'win'
       if ((type.eq.'qsp').and.(icol.eq.1))  name = 'q'
       if ((type.eq.'qsp').and.(icol.eq.2))  name = 'chiq_re'
       if ((type.eq.'qsp').and.(icol.eq.3))  name = 'chiq_im'
       if ((type.eq.'qsp').and.(icol.eq.4))  name = 'chiq_mag'
       if ((type.eq.'qsp').and.(icol.eq.5))  name = 'chiq_pha'

       if ((type.eq.'xmu.dat').and.(icol.eq.1))  name = 'energy'
       if ((type.eq.'xmu.dat').and.(icol.eq.2))  name = 'e_wrt0'
       if ((type.eq.'xmu.dat').and.(icol.eq.3))  name = 'k'
       if ((type.eq.'xmu.dat').and.(icol.eq.4))  name = 'mu'
       if ((type.eq.'xmu.dat').and.(icol.eq.5))  name = 'mu0'
       if ((type.eq.'xmu.dat').and.(icol.eq.6))  name = 'chi'

       if ((type.eq.'chi.dat').and.(icol.eq.1))  name = 'k'
       if ((type.eq.'chi.dat').and.(icol.eq.2))  name = 'chi'
       if ((type.eq.'chi.dat').and.(icol.eq.3))  name = 'mag'
       if ((type.eq.'chi.dat').and.(icol.eq.4))  name = 'phase'

       if ((type.eq.'feff').and.(icol.eq.1))  name = 'k'
       if ((type.eq.'feff').and.(icol.eq.2))  name = 'cphase'
       if ((type.eq.'feff').and.(icol.eq.3))  name = 'mag'
       if ((type.eq.'feff').and.(icol.eq.4))  name = 'phase'
       if ((type.eq.'feff').and.(icol.eq.5))  name = 'redfactor'
       if ((type.eq.'feff').and.(icol.eq.6))  name = 'lambda'
       if ((type.eq.'feff').and.(icol.eq.7))  name = 'realp'
       if ((type.eq.'feff.dat').and.(icol.eq.1))  name = 'k'
       if ((type.eq.'feff.dat').and.(icol.eq.2))  name = 'cphase'
       if ((type.eq.'feff.dat').and.(icol.eq.3))  name = 'mag'
       if ((type.eq.'feff.dat').and.(icol.eq.4))  name = 'phase'
       if ((type.eq.'feff.dat').and.(icol.eq.5))  name = 'redfactor'
       if ((type.eq.'feff.dat').and.(icol.eq.6))  name = 'lambda'
       if ((type.eq.'feff.dat').and.(icol.eq.7))  name = 'realp'
       return
       end

       subroutine glob(str, arr, marray, out, mout, nout)
c
c  support a simple (dos-like) glob mechanism: list matches to '*'
c  str will select a set of elements in array.
c  so if str does not contain '*', it is expected to be an element
c  and is returned itself.
c
c  otherwise, all elements of array that match the pattern are
c  put into the out array.
c
       implicit none
       integer  marray, mout, nout
       character*(*) str, arr(marray), out(mout)
       character*256 str1, str2
       logical    match
       integer  istar, itlen, lstr1, lstr2, istr1, istr2, j
       integer  istrln, intxt
       external istrln

       istar = index(str, '*')
       nout  = 0
       istr1 = 0
       istr2 = 0
       match = .false.
       itlen = istrln(str)
       if (istar.eq.0) then
          out(1) = str
          nout   = 1
       else
          str1  = str(1:istar-1)
          str2  = str(istar+1:itlen)
          lstr1 = istrln(str1)
          lstr2 = istrln(str2)
          do 100 j = 1, marray
             intxt = istrln(arr(j))
             if (intxt.ge.1) then
                match = .true.
                if (lstr1.ge.1) then
                   match = (arr(j)(:lstr1).eq.str1(:lstr1)).and.match
                endif
                if (lstr2.ge.1) then
                   match = (arr(j)(intxt-lstr2+1:intxt)
     $                  .eq. str2(:lstr2)).and.match
                endif
                if (match) then
                   nout = nout + 1
                   out(nout) = arr(j)
                endif
             endif
 100      continue
       endif
cc       print*, ' glob found ', nout ,' matches'
       return
       end
       subroutine prenam(group,suff)
c
c  give an un-grouped array name  a group name
c
c  suff    array name (suffix)         [in/out]
c  group   array group name (prefix)   [in]
c
c  if suff does not already have a prefix, and really is 
c  a variable name (does not contain '(', ')', ',', or '.'
c  or 'indarr'),  then the group name is prepended on output
c
c  if suff already has a prefix, it remains unchanged (well, 
c  it is trimmed and converted to lower case)
c  
       character*(*)  group, suff
       character*256  g, a
       logical   isvnam
       integer   ig, istrln
       external  isvnam
       a = suff
       call lower(a)
       call triml(a)

       g = group
       call lower(g)
       call triml(g)
       ig  = istrln(g)
       if (g.eq.' ')  g = 'my'
       if (.not.isvnam(g,1)) then
          call warn(1,
     $         ' *** Warning: invalid group name  -- '//g(1:ig))
          call fixnam(g,1)
          ig = istrln(g)
          call warn(1,'              replaced with -- '//g(1:ig))
       end if
       if ( (index(a,'.').eq.0) .and. (index(a,'indarr').eq.0) .and.
     $      (index(a,'(').eq.0) .and. (index(a,')').eq.0) .and.
     $      (index(a,',').eq.0) )  then
          a = g(1:ig)//'.'//a
       endif

       suff = a
       return
       end
c
       subroutine set_array(name, group, arr, npts, jcreat)
c
c put an array into global register
c this is a simple wrapper around iofarr and set_array_index
c
       include 'consts.h'
       include 'arrays.h'
       character*(*) name, group, vnam*256
       double precision arr(*)
       integer  npts, jcreat, ia, nof, i, iofarr
       external iofarr
       vnam = name
       ia   = iofarr(vnam, group, npts, jcreat)
       call set_array_index(ia,arr,npts)
       return
       end

       subroutine set_array_index(index, arr, npts)
c
c low level write to array data (overwrite array in
c global register, assuming index is known and correct)
c
c should be used with care (iff_sync only?), but will check
c for, and automatically recover from changing array sizes:
c    npts of existing array goes to 1: -> erase array
c    npts of existing array increases: -> erase and re-set 
c 
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       double precision arr(*)
       integer  index, npts, ia, i, icode(micode)
       integer  iofarr, istrln, icreat, j
       character*256 tmpfrm
       external  iofarr, istrln
       ia   = index
       if (npts .ne. narray(ia) ) then
          if (npts.le.1) then 
             tmpstr = '*** warning: erasing null array '
     $            //arrnam(ia)
             i = istrln(tmpstr)
             call warn(1, tmpstr)
             call erase_array(ia,.true.)
             return
          else if (npts .gt. narray(ia) ) then
             tmpfrm  = arrfrm(ia) 
             tmpstr  = arrnam(ia) 
             do 10 j = 1, micode
                icode(j) = icdarr(j,ia) 
 10          continue
             call erase_array(ia,.true.)
             ia = iofarr(tmpstr,blank,npts,icreat)
             arrfrm(ia)  = tmpfrm
             arrnam(ia)  = tmpstr
             do 20 j = 1, micode
                icdarr(j,ia) = icode(j)
 20          continue
          endif
       endif
       do 100 i = 1, npts
          array(nparr(ia)+i-1) = arr(i)
 100   continue
c do not force to be a constant array!
       call fixarr(ia, arrnam(ia), npts, 0)
       return
       end
c
       integer function get_array(name, group, jcreat, arr)
c
c get an array from global register
c returns npts of array, and array itself in last parameter 
c
       include 'consts.h'
       include 'arrays.h'
       character*(*) name, group, vnam*256
       double precision arr(*)
       integer  npts, jcreat, ia, nof, i, iofarr
       external iofarr

       get_array = 0
       vnam      = name
       npts      = 0
       ia        = iofarr(vnam, group, npts, jcreat)
       if (ia .gt.0) then
          get_array = narray(ia)
          do 100 i = 1, narray(ia)
             arr(i) = array(nparr(ia)+i-1)
 100      continue
       endif
       return
       end
       integer function get_array_index(index, arr)
c
c get an array from global register by internal index
c assumes that iofarr() has been run or a stored valued
c from iofarr() is reliable.
c
       include 'consts.h'
       include 'arrays.h'
       double precision arr(*)
       integer  index, i

       get_array_index = 0
       if (index .gt.0) then
          get_array_index = narray(index)
          do 100 i = 1, narray(index)
             arr(i) = array(nparr(index)+i-1)
 100      continue
       endif
       return
       end
