c#{dkcom.f:
       implicit none
       integer    mpts,mdoc,mtitle,mvarys, npts,ndoc,ntitle
       integer    iencol,imucol,iatz, npad, numvar
       parameter (mpts = 2**14, mdoc = 20, mtitle = 10, mvarys=8)
       double precision egrid, e0, elow, ehigh, ewidth
       double precision epad,  xvarys(mvarys)
       double precision energy(mpts), expdat(mpts)
       double precision f2cl(mpts), f1cl(mpts)
       character*100    doc(mdoc), title(mtitle), versn*6
       character*100    inpfil, xmufil, outfil, label
       logical  active, isfeff, f2tof1
       integer  ne0, nelo, nehi, ne0ish, ne0dif, iprint
       common /dfkdat/  energy, expdat, f2cl, f1cl, xvarys,
     $      egrid, e0, elow, ehigh, ewidth, epad,
     $      npad,  npts, ndoc, ntitle, iencol, 
     $      imucol, ne0, nelo, nehi, ne0ish, ne0dif, iprint,
     $      iatz, numvar, active, isfeff, f2tof1
       common /dfkchr/  doc, title, label, inpfil, xmufil, outfil,
     $      versn
       save
c#dkcom.f}
