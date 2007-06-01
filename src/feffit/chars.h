c{chars.h: -*-fortran-*-
c character strings for feffit
       character*128  outfil(mdata), chifil(mdata), bkgfil(mdata)
       character*128  titles(mtitle, mdata), fefttl(mffttl, mfffil)
       character*128 feffil(mfffil), pthlab(mpaths), messg
       character*100 doc(maxdoc, mdata), inpfil, versn
       character*16  parnam(mpthpr), frminp, frmout, asccmt*2
       character*10  skey(mdata), skeyb(mdata), vnames(maxval)*64
       common /chars/ frminp, frmout, skey, doc, outfil, chifil,
     $      titles, pthlab, feffil, fefttl, vnames, versn,
     $      messg, parnam, bkgfil, skeyb, asccmt, inpfil
c chars.h}
