c{const.h: constants & parameters for feffit -*-fortran-*-
       integer   maxpts, mpaths, mdpths, mftfit
       integer   mvarys, maxval, mconst, micode, mpthpr, mffttl
       integer   maxdoc, mtitle, mdata, mlocal
c  mdata, mvarys greatly affect program size and could be
c  reduced for smaller machines.  (mdata = 5, mvarys = 20)
       parameter(mdata  =   16) ! number of data sets
       parameter(mvarys =  128) ! number of variables
       parameter(mdpths =  512) ! number of paths per data set
       parameter(mpaths = 1024) ! number of total paths in all paths
c                           note: (mpaths < mdata * mdpths) _is_ allowed
       parameter(maxpts = 2048) 
       parameter(mconst = 2048)
       parameter(maxval = 2048)

       parameter(mftfit = 2048)
c for feff.dat files
       integer  mffpts,  mfffil, maxleg
       parameter(mffpts = 128,  mfffil = 256, maxleg =  7)
c parameters are less important for program size
       parameter(maxdoc =  20, mtitle =   10, mffttl =   10)
       parameter(mlocal =  16, micode =   64, mpthpr =   10)
c real parameters:
       double precision  etok, zero, one, qgrid, pi, rgrid
       parameter(zero=0.d0,one=1.d0, qgrid =0.05d0)
       parameter(etok =0.2624682917d0, pi = 3.141592653589793d0)
       parameter(rgrid = 20 * pi /mftfit)

c special indices for path parameters:
c jpnull = no path param; jppath , jplabl for "path" & "label"
c rest are the numerical path params, ranging from 1 to mpthpr
       integer  jpnull, jppath, jplabl, jps02,  jpe0, jpei, jpdpha
       integer  jpdelr, jpsig2,  jp3rd, jp4th
       parameter(jpnull =-10, jppath = -2, jplabl =-1)
       parameter(jps02  =  1, jpe0   =  2, jpei   = 3, jpdpha = 4)
       parameter(jpdelr =  5, jpsig2 =  6, jp3rd  = 7, jp4th  = 8)
c const.h}



