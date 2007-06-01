c{keywrd.h -*-fortran-*-
       character*32    ckeys(mckeys), macnam(macmax)
       character*128   mcdesc(macmax), chint(mckeys)
       character*128   mcargs(mcdeep,mmcarg), mcargd(macmax,mmcarg)
       character*512   macstr(mcline)
       integer         imcptr(mcline), imacro(macmax), mac_define(4)
       integer         nmacro, nmac_stop, imac_save(mcdeep), mac_exec
       integer         imac
       common /keywrd/ ckeys, chint, macstr,
     $      macnam, mcargs, mcargd, mcdesc
       common /intmac/ imcptr, imacro, imac_save, nmacro, nmac_stop,
     $      imac, mac_define, mac_exec

c  input / output integers  (like, file luns)
       integer  nkeys, iohist, iofile, ioinp
       integer  nfiles, iunit(mfiles), iprint
       logical   histry, tabdel
       common /inout/ nkeys, iohist, iofile, ioinp, nfiles,
     $      iunit, iprint,  histry, tabdel

c  misc character strings for ifeffit
       character*64   keys(mkeys), def_command*16
       character*256  values(mkeys), lfiles(mfiles)
       character*512  tmpstr, messg
       common /chars/ keys, values, lfiles, tmpstr, messg, def_command
c}
