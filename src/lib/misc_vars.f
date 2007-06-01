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
       subroutine setsca(name, value)
c
c purpose:  set value of a named scalar variable to a real number,
c           avoiding calls to encod/decod
c
c arguments:
c      name     name of text variable to set            [in]
c      value    real value of scalar variable           [in]
c
c notes:
c    1.  if the name is not found in the current table
c        (arrnam array in common block), it will be added to table.
c
c requires:  ifeffit.h, lower
       implicit none
       include 'consts.h'
       include 'arrays.h'
       include 'encod.h'
       save
       character*(*) name, tmpnam*64
       integer  inam, ival, k, istrln
       double precision value, val
       external istrln
c
       val = value
       tmpnam = name
       k   = istrln(tmpnam)
       call lower(tmpnam(1:k))
cc       print*, 'SETSCA ', tmpnam(1:k), ' -> ', val
c look up str in list of scalar names
       do 10 inam = 1, maxsca
          if ( (scanam(inam).eq.' ').or.
     $         (scanam(inam).eq.tmpnam) ) go to 12
 10    continue
 12    continue
c
       if (val.eq.zero) then
          ival = 1
       else
          do 50 ival = 2, mconst
             if ( (consts(ival).eq.val).or.
     $            (consts(ival).eq.zero) ) go to 52
 50       continue
 52       continue
          consts(ival) = val
       end if
c set scalar name, value, and update consts array
       scalar(inam) = val
       scanam(inam) = name
       scafrm(inam) = blank
c simple encoding for assignment of constant value
       icdsca(1,inam) = jconst + ival
       icdsca(2,inam) = 0
c end  subroutine fast_setsca
       return
       end

       subroutine settxt(name, value)
c
c purpose:  set value of a named text (scalar) variable
c
c arguments:
c      name     name of text variable to set            [in]
c      value    value of text variable                  [in]
c
c notes:
c    1.  if the name is not found in the current table
c        (txtnam array in common block), it will be added to table.
c
c requires:  ifeffit.h, lower, triml
c
c copyright 1997  matt newville
c
       implicit none
       include 'consts.h'
       include 'arrays.h'
       save

       character*(*) name, value, str*256, val*256, tmp*256
       integer  j, k, istrln
       external istrln

       str = name
       call triml(str)
c text strings may begin with a '$', following ifeffit syntax
       if (str(1:1).eq.'$')  str = str(2:)
       call lower(str)
       k   = istrln(str)
       val = value
       call triml(val)
c look up str in list of text scalar names
       do 10 j = 1, maxtxt
          tmp = txtnam(j)
          call triml(tmp)
          if ((tmp.eq.' ').or.(tmp.eq.str(1:k))) go to 12
 10    continue
 12    continue
c set scalar name, value
       txtnam(j) = str(1:k)
       text(j)   = val
c end  subroutine settxt
       return
       end
       double precision function getsca(name,iwarn)
c
c purpose: get value of a scalar variable, 
c          circumventing a big "decod" call
c
c arguments:
c      name     name of scalar to get                   [in] 
c
c notes:
c    1.  if the name is not found in the current table 
c        (scanam array in common block), it will be added to table
c        and initialized to zero.
c
c requires:  ifeffit.h, encod.h, setsca, lower
c
c  copyright (c) 1998  matt newville
c  
       implicit none
       include 'consts.h'
       include 'arrays.h'
       save

       character*(*) name, tmpnam*64
       integer  i, n, istrln, iwarn

       getsca = zero
       tmpnam = name
       call lower(tmpnam)
       i = 0
c loop through known scalar names
 10    continue 
       i = i + 1
c  is this an existing variable?
       if (scanam(i).eq.tmpnam) then
          getsca = scalar(i)
c       check whether this is fitting variable 
          if ((iwarn .ge. 1).and.(icdsca(1,i) .eq. -1 )) then 
             n = istrln(tmpnam)
             call echo(' Warning: the fitting variable '//tmpnam(1:n))
             call warn(1,'  is being read internally by ifeffit.')
             call warn(1,'  this may cause unstable results.')
          endif
c  is this a blank scalar name? if so, initialize to this one
       elseif(scanam(i).eq.' ') then
          call setsca(tmpnam,zero)
       elseif (i.le.maxsca) then
          go to 10
       end if
c end  function getsca
       return
       end

       subroutine  gettxt(name, value)
c
c purpose:  get value of a text variable, given it's name
c
c arguments:
c      name     name of text variable to get            [in]
c      value    value of text variable                  [out]
c
c notes:
c    1.  if the name is not found in the current table 
c        (txtnam array in common block), it will be added to table
c        and initialized to ' '.
c
c requires:  ifeffit.h, settxt, lower, triml
c
c  copyright (c) 1998  matt newville
c
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       save

       character*(*) name, value, str*256
       integer  i
       str   = name
       value = ' '
       call lower(str)
       call triml(str)
       if (str(1:1).eq.'$') str  = str(2:)
       i = 0
c loop through known text names
 10    continue 
       i = i + 1
c  is this an existing variable?
       if (txtnam(i).eq.str) then
          value = text(i)
c  is this a blank text name? if so, initialize to this one
       elseif(txtnam(i).eq.' ') then
          call settxt(str,' ')
       elseif (i.le.maxtxt) then
          go to 10
       end if
       call triml(value)
c end  function gettxt
       return
       end
       logical function isasca(name)
c      
c purpose: determine if a named  scalar exist

c arguments:
c      name     name of scalar to get                   [in] 
c
c  copyright (c) 2000  matt newville
c  
       implicit none
       include 'consts.h'
       include 'arrays.h'
       save

       character*(*) name, tmpnam*64
       integer  i
       tmpnam = name
       isasca = .false.
       call lower(tmpnam)
       i = 0
c loop through known scalar names
 10    continue 
       i = i + 1
c  is this an existing variable?
       if (scanam(i).eq.tmpnam) then
          isasca = .true.
       elseif (i.le.maxsca) then
          go to 10
       end if
c end  function isasca
       return
       end
