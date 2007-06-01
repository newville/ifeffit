c{fefdat.h
c feff.dat information for each path
       integer iptpth(0:maxleg, mfffil), iffrec(mfffil)
       integer nlgpth(mfffil), izpth(0:maxleg, mfffil), ixpath
       double precision degpth(mfffil), refpth(mfffil)
       double precision qfeff(mffpts, mfffil)
       double precision theamp(mffpts, mfffil), thepha(mffpts, mfffil)
       double precision cphase(mffpts, mfffil), sphase(mffpts, mfffil)
       double precision realp( mffpts, mfffil), xlamb( mffpts, mfffil)
       double precision rwgpth(mfffil), ratpth(3, 0:maxleg, mfffil)
       common /ffidat/ ixpath, nlgpth, izpth, iptpth, iffrec
       common /ffddat/ rwgpth, degpth, refpth, ratpth, theamp, 
     $      thepha, qfeff, realp, xlamb
c fefdat.h}
