c{arrays.h  -*-fortran-*-
c  values for data arrays
       integer  icdarr(micode,maxarr), icdsca(micode,maxsca)
       double precision  consts(mconst)
       common /maths/ icdarr, icdsca, consts 

       integer          narray(maxarr), nparr(maxarr), npnext
       double precision array(maxheap_array), scalar(maxsca)
       double precision tmparr(maxsize_array)

       double precision arrmax(maxarr), arrmin(maxarr)
       common /arrays/  array, arrmax, arrmin, scalar,
     $                  narray, nparr, npnext
       character*96   arrnam(maxarr), scanam(maxsca), txtnam(maxtxt)
       character*256  arrfrm(maxarr), scafrm(maxsca), text(maxtxt)
       common /charry/  arrnam, scanam, txtnam, text, arrfrm, scafrm
c}
