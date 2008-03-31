c{consts.h -*-fortran-*-
       double precision pi, one, zero, third, raddeg
       double precision fa, bohr, ryd, alpinv, alphfs, clight
       double precision rgrid_dx, rgrid_x0
       complex*16 coni

       parameter (pi = 3.14159 26535 89793 23846 26433d0)
       parameter (one = 1.d0, zero = 0.d0)
       parameter (third = one/3)
       parameter (raddeg = 180 / pi)
       parameter (coni = (0.d0,1.d0))

c  kf = fa/rs with fa = (9*pi/4)**third, see Ash&Merm, pg 37
       parameter (fa = 1.919 158 292 677 512 811d0)
       parameter (bohr = 0.529 177 249d0, ryd  = 13.605 698d0)
c  fine structure alpha
       parameter (alpinv = 137.035 989 56d0)
       parameter (alphfs = one / alpinv)
c  speed of light in louck's units (rydbergs?)
       parameter (clight = 2 * alpinv)
c rgrid parameters
       parameter(rgrid_dx=0.05d0, rgrid_x0 = 8.8d0)
c}
