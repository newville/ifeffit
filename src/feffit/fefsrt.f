       subroutine fefsrt( mfiles, mpaths, feffil, iffrec, jpthff)
c
c  this sorts the list of feff files so that feff.bin files (which
c  contain multiple paths) can  be read once, and in order, no matter
c  what the user-selected order was.
c  matt newville 1997
       integer mfiles, mpaths
       integer iffrec(mfiles), jpthff(mpaths)
       character*(*)  feffil(mfiles)

       integer  maxpth, maxfil
       parameter (maxpth = 2048, maxfil = 2048)
       integer jsave(maxfil), jffpth(maxfil)
       integer jfftmp(maxfil), ifftmp(maxfil)
       character*128 tmpfil(maxfil)
       if (mfiles.gt.maxfil)
     $      call fstop( ' increase maxfil in fefsrt')
       if (mpaths.gt.maxpth)
     $      call fstop( ' increase maxpth in fefsrt')
       naxpth = min(mpaths,maxpth)
c  there are a lot of path indices here, and they get confusing.
c  here's a menu:
c     iuser   "user path index"      what the user wrote in feffit.inp
c     inpath  "internal path index"  which set of path params to use
c     ifeff   "feff path index"      which feff file to use
c     idpath  "data path index"      which internal path is this for this
c                                    data set, when summing over paths
c  idpath is the key, and gives the rest using pointers in common blocks:
c     inpath = jdtpth(idpath,idata)
c     iuser  = jdtusr(idpath,idata)
c     ifeff  = jpthff(inpath)

       do 20 i = 1, naxpth
          jsave(i) = jpthff(i)
 20    continue
       do 25 i = 1, maxfil
          tmpfil(i) = ' '
          jfftmp(i) = 0
          ifftmp(i) = 0
 25    continue
c construct jffpth array {pointing  backwards from jpthff:
c     ifeff  = jpthff(inpath)
c     inpath = jffpth(ifeff)   }
       itmp  = 0
       do 50 i = 1, mfiles
          jffpth(i) = 0
          do 30 j = 1, mpaths
             if (jpthff(j).eq.i)  then
                jffpth(i) = j
                go to 32
             end if
 30       continue
 32       continue
          if ((feffil(i).ne. ' ').and.(iffrec(i).eq.0)) then
             itmp = itmp+1
             tmpfil(itmp) = feffil(i)
             ifftmp(itmp) = 0
             jfftmp(itmp) = jffpth(i)
             feffil(i)    = ' '
          end if
 50    continue
c
c group paths by file name:
       do 100 i = 1, mfiles
          if ((feffil(i).ne. ' ').and.(iffrec(i).ne.0)) then
             itmp = itmp+1
             i1   = itmp
             tmpfil(itmp) = feffil(i)
             ifftmp(itmp) = iffrec(i)
             jfftmp(itmp) = jffpth(i)
             do 90 j = min(mfiles,i+1), mfiles
                if (feffil(j).eq.feffil(i)) then
                   itmp = itmp+1
                   tmpfil(itmp) = feffil(j)
                   ifftmp(itmp) = iffrec(j)
                   jfftmp(itmp) = jffpth(j)
                   feffil(j)    = ' '
                end if
 90          continue
             feffil(i) = ' '
c sort entries with same path names:  elements i1 to itmp:
             if (i1.lt.itmp) then
                n = itmp - i1 + 1
                call sort2i(n,ifftmp(i1),jfftmp(i1))
             end if
          end if
 100   continue
c replace original arrays with new, sorted values
       do 500 i = 1, mfiles
          feffil(i) = tmpfil(i)
          iffrec(i) = ifftmp(i)
 500   continue
       do 700 i = 1,naxpth
          if (jsave(i).ne.0) jpthff(i) = jsave(jfftmp(jsave(i)))
 700   continue

       return
       end


