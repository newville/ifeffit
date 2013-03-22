#!/bin/sh

. devel_init.sh

pglink="-L$TOP/local/lib -lpgplot_iff -lpng -lz -F$TOP/local/lib -framework aquaterm -L/usr/X11R6/lib -lX11 -Wl,-framework -Wl,Foundation"


cd $TOP/work/ifeffit/
#rm -rf ifeffit-1.2.12
#tar xvzf ../Sources/ifeffit-1.2.12.tar.gz 


echo "./configure --prefix=$TOP/local -with-termcap-link=-ltermcap --with-arraysize=huge  --with-pgplot-link="$pglink""
./configure --prefix=$TOP/local -with-termcap-link=-ltermcap --with-arraysize=huge  --with-pgplot-link="$pglink"

make all
make install

cp -pr src/lib/libifeffit.so $local/lib/libifeffit.dylib
ln -s $local/lib/libifeffit.dylib    $local/lib/libifeffit.so


