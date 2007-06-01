       integer function iff_eval(string,prefix,arr,narr)
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
c  evaluate a string as an array, 
c  return values: 
c     -1    string = undef (ie, no array specified)
c      0    string given, but didn't evaluate as an array (ie, is a scalar)
c      1    string given, evaluated as array
c  arguments:
c      string   string to evaluate in array context        [in]
c      prefix   group name to use for simple array names   [in]
c      arr      array of numerical values                 [out]
c      narr     length of arr                             [out]
c
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'encod.h'
       save
       character*(*) string, prefix, str*2048, pre*256
       logical  isarr, ic_is_arr, isnum
       double precision arr(*)
       integer  icode(micode), jcode(micode), get_array
       integer  j, i, narr, ier, encod, istrln
       external get_array, ic_is_arr, encod, isnum, istrln

c initialization
       iff_eval = -1
       narr  = 0
       arr(1)= zero
       arr(2)= zero
c
c test string
       str = string
       call sclean(str)
       if ((str.eq.' ').or.(str.eq.undef)) return
       call undels(str)
       call lower(str)
       call triml(str)
       iff_eval  = 0    
c first check if number is a constant
       if (isnum(str)) then
          call str2dp(str,arr(1), ier)
          narr      = 1
          return
       endif
c next check for a named array
       pre = prefix
       call triml(pre)
       call lower(pre)
       if ((pre.ne.undef).and.(pre.ne.'')) then
          narr  = get_array(str, pre, 0, arr)
cc          print*, '  iff_eval get_arry :', narr, ' ' , str(1:30)
          if (narr .gt. 1) then
             iff_eval = 1
             return
          endif
       endif
c last resort:  encod/decod string
       ier = encod(str, jcode, icode)
       call decod (icode, micode, consts, scalar, array,
     $      narray, nparr, maxsize_array,  maxarr,  narr, arr)
       isarr = ic_is_arr(icode,micode).and.(narr .gt. 1)
       if (isarr) iff_eval = 1
       return
       end
c
       integer function iff_eval_dp(str,dpval)
c  evaluate a string as a double precision number
c  if str cannot be a number, ier < 0 is returned.
       implicit none
       include 'consts.h'
       character*(*) str
       double precision arr(maxsize_array), dpval
       integer  narr,  ix, iff_eval
       external iff_eval

       iff_eval_dp = -1
       dpval = zero
       ix =  iff_eval(str,'',arr,narr)
       if (ix .ge. 0) then
          iff_eval_dp   = 0
          dpval = arr(1)
       endif
       return
       end
       integer function iff_eval_in(str,inval)
c
       character*(*) str
       double precision dpval
       integer  iff_eval_dp, inval
       external iff_eval_dp
       inval       = 0
       iff_eval_in = iff_eval_dp(str,dpval)
       if (iff_eval_in.eq.0)   inval = int(dpval)
       return
       end
       integer function iff_eval_re(str,val)
c
       character*(*) str
       double precision dpval
       real     val
       integer  iff_eval_dp
       external iff_eval_dp
       val       = 0
       iff_eval_re = iff_eval_dp(str,dpval)
       if (iff_eval_re.eq.0)   val = dpval
       return
       end

