       program feffit
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c ---  program feffit
c
c  author   Matthew Newville
c  post     GSECARS, Bldg 434A
c           APS, Argonne National Laboratory
c           Argonne, IL 64309 USA
c  voice    (630) 252-0431
c  fax      (630) 252-0443
c  e-mail   newville@cars.uchicago.edu
c  web      http://cars.uchicago.edu/~newville/feffit/
c
c  version  feffit 2.981
c  update   25-apr-2003
c
c --- copyright 2002,2003  matt newville
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c  feffit will fit the results of a feff (5 and higher) calculation
c  to xafs chi(k) data, allowing parameters in the xafs equation to
c  vary until the least-squares difference between data and theory
c  is minimized.  principle features of feffit are:
c    -  fitting can be done in r-space or backtransformed k-space.
c    -  the xafs equation is evaluated as a sum over paths.
c    -  physical parameters for each path can be easily constrained.
c       the user writes math expressions for these parameters in
c       terms of user-chosen variables which are used in the fit.
c    -  error analysis is done, giving an estimate for the
c       uncertainties in the fit variables, the correlations
c       between these variables, and the goodness-of-fit.
c
c  feffit uses the following inputs:
c      1. an input file named feffit.inp.
c      2. a set of feffnnnn.dat files from feff (5 or higher),
c         for the xafs contribution from a scattering path.
c      3. chi(k) data, which can be in either a uwxafs 'chi' file
c         or an ascii column file.
c  see documentation for further details.
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c version notes:
c     vax versions differ from standard versions by:
c     1. irecl = 128 (standard irecl = 512) for uwxafs binary files
c     2. output log files are opened with status = 'new'
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c major revisions (for a complete revision record, contact matt) :
c    feffit 2.01 july 28 1993 (fit multiple data sets)
c    feffit 2.05 jan  19 1994 ("local"s added, many minor fixes)
c    feffit 2.10 may  15 1994 (added bkg spline option)
c    feffit 2.20 june  1 1994 (fixed bug in uncertainties)
c    feffit 2.30 nov  17 1994 (added fitting in q-space)
c    feffit 2.31 dec   4 1994
c    feffit 2.32 dec  12 1994 (added routine testrf)
c    feffit 2.33 may  10 1995 (added flag for tranquada correction)
c       - minor debugging for g77
c       - fixed rfact
c    feffit 2.34 june  3 1995 (altered inpdat, inpcol, and testrf)
c       - made to accept less strict formatting of ascii data
c    feffit 2.35 june  4 1995 (add constant phase shift)
c       - altered all path parameter handling, using "pointers"
c    feffit 2.36 june 16 1995 (add rm2fg flag)
c       -  to use {reff}^{-2} in xafs eq. instead of {reff+delr}^{-2}
c    feffit 2.37 june 22 1995 (added fixicd, rearranged decoding)
c       -  added routine fixicd to sort integer icodes, so that
c          variables come first, then trivial constants, then locals,
c          and finally the user-defined functions, ordered so that
c          they can be stably determined in one pass.  also altered the
c          handling of local "set" values so that the icdval array
c          holds the pointer to which expression to evaluate.
c   2.38 june 22 1995 (adding derived uncertainties in
c                        used defined functions and path params)
c        .f july 10  changed tranquada correction to integer flag
c        .g july 18  changed tranquada correction to real factor
c   2.39a 31-july-95 altered calculation of the number of
c                    independent points.  now considers amount of
c                    information to be a non-integer, and uses
c                    simple (2/pi) * dk * dr + 2 .
c       b 03-aug-95  fixed serious problem of not resetting
c                    values of xguess after reordering the
c                    variables and "set" values.  easy fix.
c   2.40a 17-aug-95  changed chipth, so that now, as in feff,
c                        p = Re(p) + i / lambda
c                    (was using a "-" before!).
c                    this turned out to be very important for
c                    large disorder!!!!
c       b 27-aug     changed default rm2flag to .false.
c   2.41a 04-oct-95  changed bkgfile to subtract bkg from data
c   2.41b 06-oct-95  altered fftfit to prevent access violation
c                    of qgrid on call to fftout
c   2.42  08-oct-95  added flag "kfull" to write out the full
c                    complex chi(k) (real and imag) for the
c                    theory, without doing 2 fourier tranforms
c                    (not well tested with uwxafs files yet)
c   2.42b 07-may-96  altered outcol & outdat (and xfsout) to
c                    allow arbirtary comment character and
c                    a fixed number of doc lines
c   2.45  31-may-96  added logical flags "pcfit" and "pcout" to
c                    fit and write outputs for phase corrected
c                    FT.  This will use the phase dependence of
c                    the first feffnnnn.dat file,  to alter the
c                    complex chi(k) to be
c                       chi(k)_pc = chi(k) * exp(-i * phase(k))
c                    where phase(k) is the total phase from the
c                    feffnnnn.dat file (col 2 + col 4).
c   2.45b 07-Jun-96  several minor cosmetic changes, including
c                    commenting out all "dafs" references, and
c                    improving and moving around some error
c                    checking and messages.
c   2.45c 18-Jul-96  altered output statements in fitck2
c   2.45d 05-Aug-96  use juser for output path file name,
c                    added routine setsys to set system flags
c   2.45e 07-Aug-96  fixed error in q-space fitting
c                    (number of fit points was incorrect!!)
c   2.46  02-oct-96  chipth made more efficient, dafs removed 
c   2.46b 10-oct-96  added routine fixstr for strings in fitinp,
c                    removed all traces of dafs, changed some 
c                    error messages, and changed toler
c   2.46c 11-oct-96  added routine finmsg to
c                    deal with error messages from fitinp
c   2.50   11-dec-96 added support for feff.bin files written by
c                    feff702m (matt's asci feff.bin file)
c   2.51   16-may-97 ft window arrays now calculated once (in fitdat) 
c                    and stored for use in xafsft. altered routines 
c                    fitfft, xafsft, xfsout, fitout, fitnls, fitfun
c   2.52   20-aug-97 several minor changes for ifeffit compatibility
c       b  16-dec-97 re-implemented "min" and "max" functions!
c       c  17-dec-97 increased sizes for math parsing:
c                       maxval in encod.f   ->  128
c                       micode in const.h   ->   50
c                       strings in fitinp.f -> *128
c       d  06-jan-98 fixed initialization of iunit array in getcom, 
c                    which was preventing include files from working
c       e  13-mar-98 fixed bug introduced in 2.52 for determining feff
c                    path to use for phase-correction. "pcout=t,pcfit=f"
c                    now verified to work.
c       f  25-jun-98 fixed bug in xafsft allowing 0**0.
c       g  26-jun-98 increased line lengths to 128 characters many places, 
c                    and to 256 in others.  128 char lines in feffit.inp
c                    should now always work.  Also increased micode 
c                    and several other params in encod and friends to
c                    allow many more math  expressions. Increased max 
c                    number of paths (mpaths and mdpths) to 512.  
c       h  03-mar-99 fixed bug in fitout naming scheme that could cause
c                    outputs to be written to standard output.
c   2.53   10-mar-99 altered 'nfit' in fitfun for k-space fits to be 
c                    2x number of points.  This effectively makes the 
c                    'real' component of chi(k) 0.00.
c                    -- added 'fit_space' keyword which takes the values
c                       'k', 'r', or 'q'.
c   2.54   06-apr-99 made several alteration to encod and friends to
c                    allow 256-element math expressions (possibly more)
c                    added (but commented out) rpndmp to dump the rpn 
c                    code to screen.
c   2.54   06-apr-99 made several alteration to encod and friends to
c   2.55   07-feb-02 fix parse error for min() and max() functions. 
c   2.97   05-mar-02 included in ifeffit
c   2.98   18-sep-02 fixed bug in fixicd making 'debye(20,20)' look like
c                    a constant so that set parameters and path params
c                    depending on this value were not always updated.
c   2.983  08-oct-03 use get_inpfile for command-line input file name
c----------------------------------------------------------------------
       include 'fitcom.h'
c  local variables
       character*10 systm
       integer    il, istrln
       external   istrln
cc       data systm /'unix'/
       data systm /'unix'/
c system options:  'unix','vax','dos','mac'
       call sca_init
       call echo_init
       call open_echofile('feffit.run')
       call fstop_init('feffit.err')

       call setsys(systm,vaxflg,dosflg,macflg)
c version & date
       il    = max (1,istrln(systm))
       versn = '  feffit 2.984 10-Jan-2004'
       il    = istrln(versn)
       call echo(versn(1:il))
c initialize
       call fitint
c read input file
       call echo('  - reading inputs ' )
       call fitinp
c read data files
       call echo('  - reading input data files ' )
       call fitdat
c check integer coding of math formulas
       call echo('  - checking math expressions ')
       call fitchk
c read feff files
       call echo('  - reading feff data files')
       call fefsrt( mfffil, mpaths, feffil, iffrec, jpthff)
       call fefinp( mffpts, mfffil, mffttl, maxleg, fefttl, feffil,
     $      iffrec, degflg, degpth, refpth, rwgpth, ratpth, theamp, 
     $      thepha, qfeff, cphase, sphase,
     $      realp,  xlamb, nlgpth,  izpth, iptpth)
       
c check that initial guesses for path parameters are "reasonable"
       call fitck2
c do non-linear-least-squares fit to determine best-fit values and
c uncertainties in fitted parameters
       call echo('  - finding best-fit values for the variables')
       call fitnls
c write results to log file and parameter file
       messg = '  - writing results to log file '
       il    = istrln(messg)
       if (prmout) call append(messg,' and parameter file',il)
       call echo (messg(1:il))
       call fitlog
       if (prmout) call fitprm
c write results to data files:
       if (noout) then
          call echo('  - *not* writing output data files')
       else
          call echo('  - writing output data files')
          call fitout
       endif
c finished
       call echo('  feffit is finished.  have a nice day.')
       call close_echofile()
c end program feffit
       end
