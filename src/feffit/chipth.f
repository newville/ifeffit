       subroutine chipth(ampfef, phafef, qfeff, xlamb, realp, nffpts,
     $      reff, degen, s02, e0shft, e0imag, delpha, deltar, sigma2,
     $      third, fourth, tranq, rm2flg,
     $      nqvals, mchiq, chiqr, chiqi)
c
c  evaluate the theoretical chi(k) for a scattering path given:
c   1. feff information          (ampfef - degen)
c   2. the xafs path parameters  (s02 - fourth)
c
c     copyright 1993 university of washington        matt newville
c
c input:
c    ampfef   amplitude from feff for the path
c    phafef   phase shift from feff (w/o 2*k*r term) for the path
c    qfeff    k-values from feff for the other feff arrays
c    xlamb    mean-free-path from feff
c    realp    real part of p (momentum) from feff
c    nffpts   number of data points in the feff arrays
c    reff     half path length for the path
c    degen    path degeneracy (# of equivalent paths)
c    s02      constant multiplicitive amplitude factor for the path
c    e0shft   e0 shift to use for the path
c    e0imag   imaginary e0 shift / broadening for the path
c    delpha   constant phase shift (delta phase )
c    deltar   delta r to add to reff for the path
c    sigma2   debye-waller factor for the path
c    third    third cumulant for the path
c    fourth   fourth cumulant for the path
c    tranq    real paramter giving coefficient in tranquada correction
c    rm2flg   flag for using (reff)^{-2} instead of (reff+delr)^{-2}
c    nqvals   index of highest q value to calculate
c             (no greater than 512, no less than 20)
c    mchiq    array dimension for chiqr and chiqi
c output:
c    chiqr    real part \   of complex chi(k) for this path
c    chiqi    imag part /   with all path parameters applied
c **note measured chi(k) data corresponds to  chiqi **
c------------------------------------------------------------------
       integer izero, ipos, i, nqvals, nffpts, nqdata, mchiq, nqffmx
       complex*16 coni, cp, cp2, cphshf, cdwf, cargu, cchi, ciei
       double precision chiqr(mchiq), chiqi(mchiq)
       double precision e0eff, s02r2n, degen, reff, sigma2, third
       double precision s02, e0shft, e0imag, delpha, deltar
       double precision first, fourth, r2m2, qgrid, energy, q, tranq
       double precision expmax, expmin, small, etok, one
       double precision rep, xlam, cxlam, pha, amp, car
       double precision ampfef(nffpts), phafef(nffpts)
       double precision qfeff(nffpts), xlamb(nffpts),  realp(nffpts)
       logical rm2flg, le0big
       parameter (coni = (0., 1.) , one = 1.)
       parameter (small=0.0001, etok = 0.26246 82917 )
       parameter (expmax = 30., expmin = -expmax, qgrid = 1./20.)
c------------------------------------------------------------------
c  combine path parameters and feff data to get theory chi
c  note: feff writes out the feffnnnn.dat files such that
c    phase = 2*q*r + phase_shifts. additional phase shifts and
c    all amplitude reduction terms use p, the complex momentum.
c    q is used only to reproduce chi(k) from feff
       nqffmx = int(qfeff(nffpts) / qgrid) + 5
       nqdata = min(512, min(nqvals, min(nqffmx, mchiq)))
       if (nqdata.lt.25)  nqdata = 400
c tranquada correction
       first = deltar - tranq * sigma2 / reff
       r2m2  = 1 / ( (reff + deltar) * (reff + deltar) )
       if (rm2flg)  r2m2   = 1 / ( reff * reff )
       le0big  = (abs(e0shft).ge.small)
c ipos is a place holder for the routine lintrp
c izero stores the index of the q = 0. location.
       ipos   = 1
       izero  = 0
c store some multiplications before the main loopa
       e0eff  = e0shft * etok
       ciei   = coni * e0imag * etok
       s02r2n = degen * s02 * r2m2
c the official xafs calculation loop:
       do 500 i = 1, nqdata
c  e0-shift the value of q
          q   = (i - 1) * qgrid
          if (le0big) then
             energy =  q*q - e0eff
             q      = sign(one,energy) * sqrt(abs(energy))
          end if
c  q = zero is special, and will be dealt with below
          if (abs(q).le.small) then
             izero = i
          else
c  interpolate amplitude, phase, realp and xlamb of feff chi
             call lintrp(qfeff,  realp, nffpts, q, ipos,  rep)
             call lintrp(qfeff,  xlamb, nffpts, q, ipos, xlam)
             call lintrp(qfeff, phafef, nffpts, q, ipos,  pha)
             call lintrp(qfeff, ampfef, nffpts, q, ipos,  amp)
c  evaluate complex momemtum
             cp2    =  (rep + coni/max(xlam,small))**2 + ciei
             cp     =  sqrt(cp2)
c  mean free path, complex debye-waller factor and phase shift
             cxlam  =  -2*reff* dimag(cp)
             cdwf   =  -2*cp2 * (sigma2 -    cp2 * fourth/3)
             cphshf =   2*cp  * (first  - (2*cp2)* third /3) + delpha
c  create complex chi, first checking that the exponential won't crash
             cargu  =  cxlam + cdwf + coni*(2*q*reff + pha + cphshf)
             car    =  max(expmin, min(expmax, dble(cargu)))
             cchi   =  (amp * s02r2n / abs(q)) *
     $                     exp(cmplx(car, dimag(cargu)))
c  save real and imag chi for this value of q
             chiqi(i) = dimag(cchi)
             chiqr(i) = -dble(cchi)
cc             print*, i, q, rep, xlam,  pha, amp, cchi
          end if
 500   continue
c  fill in guess for data at q = zero if needed
       if (izero.eq.1)  then
          chiqr(1) = 2*chiqr(2) - chiqr(3)
          chiqi(1) = 2*chiqi(2) - chiqi(3)
       elseif (izero.ge.2) then
          chiqr(izero) = (chiqr(izero-1) + chiqr(izero+1))/2
          chiqi(izero) = (chiqi(izero-1) + chiqi(izero+1))/2
       end if
       return
c end subroutine chipth
       end
