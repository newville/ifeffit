#!/bin/sh

. devel_init.sh

cd $TOP/build
tar xvzf ../Sources/libpng-1.2.44.tar.gz
cd libpng-1.2.44
./configure --prefix=$TOP/local --enable-shared=no --enable-static=yes --disable-dependency-tracking
make
make install
