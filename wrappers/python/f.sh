gcc  -Wl,-framework -Wl,Foundation -bundle -framework Python \
build/temp.darwin-7.3.0-Power_Macintosh-2.3/ifeffit_wrap.o \
-o build/lib.darwin-7.3.0-Power_Macintosh-2.3/_ifeffit_1.so \
-L/Applications/Ifeffit/lib -lifeffit -lpgplot_iff  -lpng_iff -lz_iff -laquaterm \
-L/usr/X11R6/lib -lX11 -L/sw/lib -L/usr/lib  -lfrtbegin -lg2c -lgcc -lSystem


gcc  -Wl,-framework -Wl,Foundation  -bundle -framework Python \
build/temp.darwin-7.3.0-Power_Macintosh-2.3/ifeffit_wrap.o \
-L/Applications/Ifeffit/lib -L/Applications/Ifeffit/pgplot \
-L/sw/lib -L/usr/lib -L/usr/X11R6/lib -lifeffit -lpgplot_iff \
-lpng_iff -lz_iff -laquaterm -lg2c -lgcc -lX11 -lfrtbegin -lg2c -lgcc -lSystem \
-o build/lib.darwin-7.3.0-Power_Macintosh-2.3/_ifeffit.so
