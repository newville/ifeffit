       subroutine txpath
       include 'fitcom.h'
       integer nstart, id, ipath, i, il, inpath, jfeff, juser

       id = 1
c       print*, ' tx 1'
       do ipath = 1, 4
          inpath    = jdtpth(ipath,id)
c          print*, '  path ', inpath, ipath, id
          if (inpath.gt.0) then
             jfeff  = jpthff(inpath)
c             print*, ' JFEFF  = ', jfeff, inpath, ipath,
c     $            refpth(jfeff)
          endif
       enddo

       return 
       end
