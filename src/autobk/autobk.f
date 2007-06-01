       program autobk
c
c  autobk   version 2.92b  07-Dec-2000
c
c  author   Matthew Newville,  The University of Chicago
c  e-mail   newville@cars.uchicago.edu
c  post     GSECARS, Bldg 434A
c           APS, Argonne National Laboratory
c           Argonne, IL 64309 USA
c  voice    (630) 252-0431
c  fax      (630) 252-0443
c
c further information on this code is available at
c      http://cars.uchicago.edu/~newville/autobk/
c
c --- copyright 1998,1999  matt newville
c --- copyright 1995  matt newville, university of washington
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c  autobk removes the background of x-ray-absorption fine-structure
c  data.  a spline function is used to approximate the background. 
c  the spline is chosen so that the resulting chi is optimized at 
c  low-r. the optimization minimizes the difference between the
c  data and a standard chi(r) at low-r.  the standard is used to
c  estimate the leakage form the first shell into the low-r region,
c  and since this leakage is a small portion of the first shell, the
c  standard does not need to be an extremely accurate estimate of
c  the true first shell.  the standard chi should be a chi for which
c  the background is trusted, and can be either a theoretical
c  calculation or an experimental standard.  if no standard chi is
c  specified, the low-r components of chi(r) will be minimized. 
c
c major revisions (for a complete revision record, contact matt) :
c
        include 'autobk.h'
c------------------------------------------------------------------
c   local variables
       character system*10
       logical   first, domore, dorun
       integer   iinpf, ilogf, ilen, istrln
       external  istrln
       data      first, domore, dorun /.true.,.true.,.true./ 
       data      iinpf, ilogf / 2, 4/ 
       data system /'unix'/
c system options:  'unix','vax','dos','mac'
       call setsys(system,vaxflg,dosflg,macflg,unxflg)
       call sca_init
       call echo_init
       call open_echofile('autobk.run')
       call fstop_init('autobk.err')
c      version & date
       ilen   = max(1,istrln(system))
       versn  = '   autobk:  2.941  10-Jan-2004 '
       ilen   = istrln(versn)
       call echo(versn(1:ilen))
c------------------------------------------------------------------
c loop for each different running of program
 100   continue
c     initialize variables in common blocks, open files
       call autint
c     read input file 
       call autinp(iinpf, ilogf, first, domore, dorun)
       if (dorun) then
c      read in data, subtract pre-edge, reset fitting ranges, etc
          call autdat
c      do nonlinear least-square fittings for background, and
c      possibily e0 and amplitude of theory.
          call autnls
c      write out results to log file
          call autlog(ilogf)
c      write out data results to data files
          call autout
       end if
c      continue on to next data set
       if (domore) go to 100
c----------------------------------------------------------------
c  finished: close files, and give happy ending message
       close(iinpf)
       close(ilogf)
       call echo( '   autobk is finished.')
       call echo( '                     have a nice day.')
       call close_echofile()
c  end main program autobk
       end
