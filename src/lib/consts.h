c{consts.h  -*-fortran-*-
       include 'maxpts.h'
c
       integer  maxarr, maxdoc, maxtxt
       integer  korder, maxnot, mtknot
       integer  mconst, micode, maxsca, mffpts
       integer  mwfft , maxplt, maxfft, mdata
       integer  mpthpr, mppars, mpaths
       integer  mvarys, mfffil, mffttl
       integer  maxleg, mckeys, macmax, mcline
       integer  mmcarg, mcdeep, mfiles, mkeys
       integer  maxheap_array
       integer  max_restraint
       integer  max_pathindex
       integer  max_outarr
       parameter ( mckeys =   64 )
       parameter ( macmax =  512 )
       parameter ( mcline = 4096 )
       parameter ( mcdeep =   32 )
       parameter ( mfiles =   16 )
       parameter ( max_restraint =   32)
       parameter ( mkeys  =   64 )
       parameter ( maxheap_array = maxpts*512)
       parameter ( maxarr = 8192 ) ! # of array variables
       parameter ( maxsca = 65536) ! # of scalar variables
       parameter ( maxtxt =  8192) ! # of text variables 
       parameter ( mconst = 65536) ! # of numerical constants
       parameter ( maxplt =   64 ) ! # of plots 
       parameter ( maxdoc =   20 ) ! # of docs from data file
       parameter ( micode =  256 ) ! # of elements in math icode array
       parameter ( mffpts =  128 ) ! # of points in feff arrays
       parameter ( mffttl =   10 ) ! # of feff titles
       parameter ( maxleg =    7 ) ! # of legs in feff path
       parameter ( mpthpr =   16 ) ! # of path parameters
       parameter ( maxnot =   32 ) ! # of knots in background spline
       parameter ( korder =    4 )
       parameter ( mtknot = maxnot+korder)
       parameter ( mdata  =   16 ) ! # of data sets
       parameter ( mvarys =  128 ) ! # of fitting variables
       parameter ( mppars =   16 ) ! # of path parameters
       parameter ( mmcarg =    9 ) ! # of macro arguments
       parameter ( max_pathindex = 9999) ! # path index numbder
       parameter ( max_outarr    = 99)   ! # of output columns in write_data

       parameter ( mpaths =  1024) ! # of paths, total
       parameter ( mfffil =  1024) ! # of feff files
c
c common constants
       double precision  zero, one, etok, pi, qgrid, rgrid, tiny
       parameter ( zero  = 0.d0)
       parameter ( one   = 1.d0)
       parameter ( etok  = 0.2624682917d0)
       parameter ( pi    = 3.141592653589793d0)
       parameter ( tiny  = 1.d-12)
       character  undef*8,undef_array*10, blank*1
       parameter (undef= '%undef%', blank = ' ')
       parameter (undef_array= '%_undef._%')
c
c fft constants
       parameter ( maxfft = 2048 )         ! points for fft arrays
       parameter ( mwfft  = 4*maxfft+15)
       parameter ( qgrid = 0.050d0)
       parameter ( rgrid = pi/(qgrid * maxfft))
c}
