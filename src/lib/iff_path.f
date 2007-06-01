       subroutine iff_path(str)
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
c define path parameters for a given path
c a path defintion consists of the following parameters:
c   index    -- path pointer, defaults to next available integer
c   $file    -- name of feff.dat / feff.bin, index
c   $label   -- text string
c   path.k   -- array for k when  using path.amp / path.pha
c   path.amp -- array for explicit amplitude factor
c   path.pha -- array for explicit phase shift
c   s02      -- scalar amplitude factor
c   e0       -- scalar e0 shift
c   ei       -- scalar ei shift
c
       implicit none
       include 'consts.h'
       include 'keywrd.h'
       include 'arrays.h'
       include 'fefdat.h'
       include 'pthpar.h'
       save
       
       character*(*)  str
       character*256  tmpfff, tmplab
       character*2048 parstr(mpthpr)
       character*64   defkey(3)
       double precision  getsca, xiup, xip
       integer    icode(micode), jcode(micode)
       integer   irec, istrln, i, j, k, ier, ilen, lv
       integer   ndfkey, ifeff, inpath, idpath, iupath
       integer   iff_eval_dp, iff_eval_in, encod
       logical   force_read
       external  getsca, iff_eval_dp, iff_eval_in, istrln, encod
c special indices for path parameters:
c jpnull = no path param; jppath , jplabl for "path" & "label"
c rest are the numerical path params, ranging from 1 to mpthpr

cc       print *, 'PATH ', str(1:70)
       call gettxt('feff_file',  tmpfff)
       xiup   = getsca('path_index',1)
       iupath = max(0,min(int(xiup),max_pathindex))
       do 10 i = 1, mpthpr
          parstr(i) = undef
 10    continue
       tmplab = ' '
       force_read = .false.
c  interpret keyword/value pairs for setting options
       call bkeys(str, mkeys, keys, values, nkeys)
cc       print*, 'bkeys sees ', nkeys, ' pairs '
c set default keywords for "undefined" sets
c ie: the first two unkeyed values will take keys 'id', 'feff'
       ndfkey    =  2
       defkey(1) = 'index'
       defkey(2) = 'feff'
       irec      =  0
       do 100 i = 1, nkeys
          k = istrln( keys(i))
          lv = istrln(values(i))
cc          print*, 'arg: ', keys(i)(:k), ' -> ', values(i)(:lv)
          if ((values(i).eq.undef).and.(i.le.ndfkey)) then
             values(i) = keys(i)
             keys(i)   = defkey(i)
          end if
          if ((keys(i).eq.'feff').or.(keys(i).eq.'file'))  then 
             if (values(i).ne.undef) then
                parstr(jfppth) = values(i)
             endif
          elseif (keys(i).eq.'label') then 
             tmplab = values(i)
          elseif (keys(i).eq.'index') then 
             iupath = -1
             ier    = iff_eval_in(values(i), j)
             if ((ier.eq.0).and.(j.gt.0).and.
     $            (j.le.max_pathindex)) then
                xip    = j  * one
                iupath = j
                call setsca('path_index', xip)
             endif
c  key for feff.bin
          elseif ((keys(i).eq.'key').or.(keys(i).eq.'record')) then 
             ier = iff_eval_in(values(i), j)
             if ((ier.eq.0).and.(j.ge.0)) irec = j
          elseif ((keys(i).eq.'s02').or.
     $            (keys(i).eq.'amp')) then 
             parstr(jfps02) = '('//values(i)(:lv)//')'
          elseif (keys(i).eq.'e0') then 
             parstr(jfpe0)  = '('//values(i)(:lv)//')'
          elseif (keys(i).eq.'ei') then 
             parstr(jfpei)  = '('//values(i)(:lv)//')'
          elseif (keys(i).eq.'delr') then 
             parstr(jfpdr)  = '('//values(i)(:lv)//')'
          elseif (keys(i).eq.'degen') then 
             parstr(jfpdeg)  = '('//values(i)(:lv)//')'
          elseif ((keys(i).eq.'sigma2').or.
     $            (keys(i).eq.'sig2')) then 
             parstr(jfpss2) = '('//values(i)(:lv)//')'
          elseif (keys(i).eq.'third') then 
             parstr(jfp3rd) = '('//values(i)(:lv)//')'
          elseif (keys(i).eq.'fourth') then 
             parstr(jfp4th) = '('//values(i)(:lv)//')'
          elseif (keys(i).eq.'dphase') then 
             parstr(jfppha) = '('//values(i)(:lv)//')'
          elseif (keys(i).eq.'k_array') then 
             parstr(jfpkar) = '('//values(i)(:lv)//')'
          elseif (keys(i).eq.'phase_array') then 
             parstr(jfppar) = '('//values(i)(:lv)//')'
          elseif (keys(i).eq.'amp_array') then 
             parstr(jfpaar) = '('//values(i)(:lv)//')'
          elseif (keys(i).eq.'force_read') then 
             call str2lg(values(i),force_read,ier)
          else
             k = istrln( keys(i))
             messg = keys(i)(1:k)//' " will be ignored'
             call warn(1,' *** path: unknown keyword " '//messg)
          end if
 100   continue 
c
c a user path index must be given
       if (iupath.le.0) then
          call warn(2,'  path: no valid path index given')
          return
       end if
c
c decide what internal path to use for this user path index
c
c     iupath  user path index      user-chosen index
c     inpath  internal path index  which set of path params to use           
c     ifeff   feff path index      which feff file to use
c     idpath  data path index      which internal path is this for this
c                                  data set, when summing over paths
c  idpath is the key, giving the rest using pointers:
c     inpath = jdtpth(idpath) 
c     iuser  = jdtusr(idpath)
c     ifeff  = jpthff(inpath)
c
c  so first, get idpath:
       idpath = 0
 200   continue 
       idpath = idpath + 1
       if (idpath.gt.mpaths) then
          call warn(2, ' path -- too many paths for a data set')
          return
       elseif (jdtusr(idpath).eq.0) then
          jdtusr(idpath) =  iupath
       elseif (jdtusr(idpath).ne.iupath) then
          go to 200
       end if
c
c now get inpath so that jdtpth(idpath) = inpath
c      inpthx stores the current maximum path index
       if (jdtpth(idpath).eq.0) then
          inpthx  = inpthx + 1
          jdtpth(idpath)= inpthx
       end if
       inpath = jdtpth(idpath)

c 
c but wait, there's more: get jfeff, if we know it. 
c  encod and store math formula for each path param
c
c   assign a feff path to the internal path:
c-- ifeff set here          so that jpthff(inpath) = ifeff
       if (parstr(jfppth).ne.undef) then
          tmpfff = parstr(jfppth)
          do 1640 ifeff = 1, mfffil
             if ( (feffil(ifeff) .eq. tmpfff).and.
     $            (iffrec(ifeff) .eq. irec)) go to 1700
             if (  feffil(ifeff) .eq. blank ) go to 1700
 1640     continue
c 1680     continue
c          itfeff        = itfeff  + 1
c          ifeff         = itfeff
 1700     continue
          jpthff(inpath)= ifeff
          jusedg(inpath)= 1
          feffil(ifeff) = tmpfff
          iffrec(ifeff) = irec
          if (force_read) lffred(ifeff) = .false.
       end if
c        print*, 'PATH: iupath, idpath | inpath, iuser, ifeff ',
c      $      iupath, idpath, jdtpth(idpath) ,
c      $      jdtusr(idpath),  jpthff(inpath)

c
c OK, so now we set all the parameters
       pthlab(inpath) = tmplab
c path id not set until the path is actually read....
       do 2800 i = 1, mpthpr
          if ((i.ne.jfppth).and.(parstr(i).ne.undef)) then
cc             call rmquot(parstr(i))
c             print*, 'path set param: ', i, '-> ', 
c     $            parstr(i)(:istrln(parstr(i)))             

             ier = encod(parstr(i), jcode, icode)
             if (iprint.ge.8) call rpndmp(jcode)
             do 2700 j  = 1, micode
                icdpar(j,i,inpath) = icode(j)
 2700        continue 
          end if
 2800  continue 
c
       return
       end
