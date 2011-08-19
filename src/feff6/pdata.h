cc -*-fortran-*-
c
c common declarations for phases and potentials data,
c shared by some codes.  formerly used a common block...
c
      character*80 text(40), title(5)
      character*6  potlbl(0:npotx)
      complex*16 ph(nex,ltot+1,0:npotx)   !complex phase shifts
      complex*16 eref(nex)  	          !complex energy reference
      double precision rat(3,0:legtot+1)  !position of each atom, code units(bohr)
      double precision em(nex) !energy mesh
c r, beta, eta for each leg
      double precision ri(legtot), beta(legtot+1), eta(0:legtot+1)
      double precision deg, rnrmav, xmu, edge !(output only)
      integer lmax(nex,0:npotx)  !max l with non-zero phase for each energy
      integer ipot(0:legtot)     !potential for each atom in path
      integer iz(0:npotx)        !atomic number (output only)
      integer ltext(40), ltitle(5) !length of each string
c nscatters, nlegs (nleg = nsc+1), npotentieal, energy points,
c index of energy grid corresponding to k=0 (edge)
c index of current path (output only)
c l0, il0 = final and lfinal+1 (used for indices)
c lmaxp1 =  largest lmax in problem + 1

      integer nsc, nleg, npot, ne, ik0, ipath, ihole
      integer l0, il0, lmaxp1, ntext, ntitle
