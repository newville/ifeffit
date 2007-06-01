c{fefdat.h -*-fortran-*-
c feff.dat information for each path
c
c    lffred:  is feff file/record read yet?
       logical lffred(mfffil)
c  iffused: number of real paths using this feff data
       integer iffused(mfffil)
       integer iptpth(0:maxleg, mfffil), iffrec(mfffil)
       integer nlgpth(mfffil), izpth(0:maxleg, mfffil)
       integer nffpts(mfffil)
       double precision  degpth(mfffil), refpth(mfffil), rwgpth(mfffil)
       double precision  qfeff(mffpts, mfffil)
       double precision  theamp(mffpts, mfffil)
       double precision  thepha(mffpts, mfffil) 
       double precision  realp( mffpts, mfffil) 
       double precision  xlamb( mffpts, mfffil)
       double precision  thcaps(mffpts, mfffil), thsaps(mffpts, mfffil) 
       double precision  ratpth(3, 0:maxleg, mfffil)
       common /fefdat/ lffred, nlgpth, izpth, iptpth, iffrec, iffused,
     $      nffpts, rwgpth, degpth, refpth, ratpth, qfeff,
     $      theamp, thepha, realp, xlamb, thcaps, thsaps
       
       character*256   feffil(mfffil), fpthid(mfffil)
       character*80    fefttl(mffttl,mfffil)
       common /fefchr/ feffil, fefttl, fpthid
c fefdat.h}
