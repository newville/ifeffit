       subroutine chipth(ampfef, phafef, qfeff, xlamb, realp, nfxpts,
     $      reffin,  nkpar, akpts, aamp,apha, mchiq,chiqr,chiqi)
c
c//////////////////////////////////////////////////////////////////////
c Copyright (c) 1997--2000 Matthew Newville, The University of Chicago
c Copyright (c) 1992--1996 Matthew Newville, University of Washington
c
c Permission to use and redistribute the source code or binary forms of
c this software and its documentation, with or without modification is
c hereby granted provided that the above notice of copyright, these
c terms of use, and the disclaimer of warranty below appear in the
c source code and documentation, and that none of the names of The
c University of Chicago, The University of Washington, or the authors
c appear in advertising or endorsement of works derived from this
c software without specific prior written permission from all parties.
c
c THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
c EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
c MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
c IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
c CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
c TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
c SOFTWARE OR THE USE OR OTHER DEALINGS IN THIS SOFTWARE.
c//////////////////////////////////////////////////////////////////////
c
c  evaluate the theoretical chi(k) for a scattering path given 
c  information from feff arrays  (ampfef .. degen) and xafs path 
c  parameters  (s02 .. apha) 
c
c input:
c    ampfef   amplitude from feff for the path
c    phafef   phase shift from feff (w/o 2*k*r term) for the path
c    qfeff    k-values from feff for the other feff arrays
c    xlamb    mean-free-path from feff
c    realp    real part of p (momentum) from feff
c    nfxpts   number of data points in the feff arrays
c    reff     half path length for the path
c    nkpar    # of k-points for k, phase, and array path parameters
c    akpts    array of k-points for array path parameters
c    aamp     array of amplitude-points for array path parameters
c    apha     array of phase-points for array path parameters
c    mchiq    array dimension for chiqr and chiqi
c output:
c    chiqr    real  \   part of complex chi(k) for this path
c    chiqi    imag  /   with all path parameters applied
c **note measured chi(k) data corresponds to  chiqi **
c
c in param() (from pthpar.h):
c    degen    path degeneracy (# of equivalent paths)
c    s02      constant multiplicitive amplitude factor for the path
c    e0shft   e0 shift to use for the path
c    e0imag   imaginary e0 shift / broadening for the path
c    delpha   constant phase shift (delta phase )
c    deltar   delta r to add to reff for the path
c    sigma2   debye-waller factor for the path
c    third    third cumulant for the path
c    fourth   fourth cumulant for the path
c
c------------------------------------------------------------------
       implicit none
       include 'consts.h'
       include 'pthpar.h'
       integer     izero, ipos, i, nfxpts, nqdata, mchiq, nqffmx
       integer     nkpar, nkpos
       complex*16        coni, cp, cp2, cphshf, cdwf, cargu, cchi, ciei
       double precision  akpts(*), aamp(*), apha(*), tmpa, tmpp
       double precision  pparams(10)
       double precision  chiqr(mchiq), chiqi(mchiq)
       double precision  e0eff, s02r2n, degen, reffin, reff
       double precision  s02, e0shft, e0imag, delpha, deltar, sigma2
       double precision  first, fourth, r2m2,  energy, q, third
       double precision  small,  half, expmax, expmin
       double precision  rep, xlam, cxlam, pha, amp, car, x3rd, x4th
       double precision  ampfef(nfxpts), phafef(nfxpts), qpos
       double precision  qfeff(nfxpts), xlamb(nfxpts),  realp(nfxpts)
       logical  le0big, iskpar
       parameter (coni = (0.d0, 1.d0) )
       parameter (small=1.d-6, half =0.5d0)
       parameter (expmax= 85.d0, expmin = -expmax)
c------------------------------------------------------------------
       degen  = param(jfpdeg)
       s02    = param(jfps02)
       e0shft = param(jfpe0)
       e0imag = param(jfpei)
       delpha = param(jfppha)
       deltar = param(jfpdr)
       sigma2 = param(jfpss2)
       third  = param(jfp3rd)
       fourth = param(jfp4th)

c  combine path parameters and feff data to get theory chi
c  note: feff writes out the feffnnnn.dat files such that
c    phase = 2*q*r + phase_shifts. additional phase shifts and
c    all amplitude reduction terms use p, the complex momentum.
c    q is used only to reproduce chi(k) from feff
       nqffmx = int((qfeff(nfxpts) + 1.d0) / qgrid) + 1
       nqdata = min(nqffmx, mchiq)
c       print*, 'chipth: nqffmx, nqdata, nqvals, mchiq'
c       print*,  nqffmx, mchiq, nqdata

c tranquada correction
       reff   = max(small, reffin)
       first  = deltar - 2 * sigma2 / reff
       r2m2   =  1 / (reff + deltar)**2
       le0big = (abs(e0shft).ge.small)
       iskpar = nkpar.gt.0
cc       print*, ' CHIPTH ', nkpar, iskpar
c ipos is a place holder for the routine lintrp
c izero stores the index of the q = 0. location.
       ipos   = 1
       nkpos  = 1
       izero  = 0
c store some multiplications/divisions before the main loopa
       e0eff  = e0shft * etok
       ciei   = coni * e0imag * etok
       s02r2n = degen * s02 * r2m2
       x4th   = fourth / 3
       x3rd   = 2 * (third / 3)

c the official xafs calculation loop:
       do 500 i = 1, nqdata
c  e0-shift the value of q
          q   = (i - 1) * qgrid
          if (le0big) then
             energy = q*q - e0eff
             q      = sign(one,energy) * sqrt(abs(energy))
          end if
c  q = zero is special, and will be dealt with below
          if (abs(q).le.small) then
             izero = i
          else
c  cubic spline interpolatation of 
c  amplitude, phase, realp and xlamb of feff chi
             call hunt(qfeff,nfxpts,q,ipos)
             qpos = 0
             if (abs(qfeff(ipos+1) - qfeff(ipos)).gt.small) then
                qpos = (q-qfeff(ipos))/(qfeff(ipos+1)-qfeff(ipos))
             endif
             pha = phafef(ipos) + qpos*(phafef(ipos+1) -phafef(ipos))
             amp = ampfef(ipos) + qpos*(ampfef(ipos+1) -ampfef(ipos))
             rep = realp(ipos)  + qpos*( realp(ipos+1) - realp(ipos))
             xlam= xlamb(ipos)  + qpos*( xlamb(ipos+1) - xlamb(ipos))
c  k-dependent path parameters
             if (iskpar) then
                call hunt(akpts,nkpar,q,nkpos)
                nkpos = min(nkpar-1,max(1,nkpos))
                qpos = 0
                if (abs(akpts(nkpos+1) - akpts(nkpos)).gt.small) then
                   qpos = (q - akpts(nkpos))/
     $                  (akpts(nkpos+1)-akpts(nkpos))
                endif
                tmpa = aamp(nkpos) + qpos*(aamp(nkpos+1) -aamp(nkpos))
                tmpp = apha(nkpos) + qpos*(apha(nkpos+1) -apha(nkpos))
                amp  = amp * tmpa
                pha  = pha + tmpp
             endif
c  evaluate complex momemtum
             cp2    =  (rep + coni/xlam)**2 + ciei
             cp     =  sqrt(cp2)
c  mean free path, complex debye-waller factor and phase shift
             cxlam  =  -2*reff* dimag(cp)
             cdwf   =  -2*cp2 * (sigma2 -  cp2 * x4th)
             cphshf =   2*cp  * (first  -  cp2 * x3rd) + delpha
c  create complex chi, first checking that the exponential won't crash
             cargu  =  cxlam + cdwf + coni*(2*q*reff + pha + cphshf)
             car    =  max(expmin, min(expmax, dble(cargu)))
             cchi   =  (amp * s02r2n / abs(q)) *
     $                     exp(dcmplx(car, dimag(cargu)))
c  save real and imag chi for this value of q
             chiqi(i) = dimag(cchi)
             chiqr(i) = -dble(cchi)
          end if
 500   continue
c  fill in guess for data at q = zero if needed
       if (izero.eq.1)  then
          chiqr(1) = 2*chiqr(2) - chiqr(3)
          chiqi(1) = 2*chiqi(2) - chiqi(3)
       elseif (izero.ge.2) then
          chiqr(izero) = (chiqr(izero-1) + chiqr(izero+1)) * half
          chiqi(izero) = (chiqi(izero-1) + chiqi(izero+1)) * half
       end if
       return
c end subroutine chipth
       end

