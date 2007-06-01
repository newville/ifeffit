
read_data(../data/cu150k.chi , group=a, type=chi)

 set b.chi = a.chi

 kweight=2, kmin=2, kmax=18, dk=2

 fftf(a.chi)
 fftf(b.chi, pc_edge='Cu k')


 newplot(a.r, a.chir_mag, key="no pc")
   plot(b.r, b.chir_mag,   key="with pc")

