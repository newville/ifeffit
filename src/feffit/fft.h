c{fft.h: -*-fortran-*-
c  parameters for fourier transforms in feffit
       double precision wfftc(4*maxpts + 15)
       double precision qwin1(mdata), qwin2(mdata)
       double precision rwin1(mdata), rwin2(mdata), rweigh(mdata)
       double precision qweigh(mdata), qmin(mdata), qmax(mdata)
       double precision rmin(mdata), rmax(mdata)
       integer  nqfit(mdata), nqpts(mdata), nrpts(mdata)
       integer  iqwin(mdata), irwin(mdata), ifft(mdata)
       integer  jffphs(mdata)
       character*32 sqwin(mdata), srwin(mdata)
       common /fft/ nqpts, iqwin, qmin, qmax, qwin1, qwin2, qweigh,
     $              nrpts, irwin, rmin, rmax, rwin1, rwin2, rweigh,
     $              nqfit, ifft, jffphs, wfftc
       common /ffts/ sqwin, srwin
c fft.h}
