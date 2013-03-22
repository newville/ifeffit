#!/bin/sh

. devel_init.sh

cd $TOP/build

tar xvzf $TOP/Sources/horae-070.tar.gz

cp -pr $TOP/Sources/Iff_Makefile.PL  $TOP/build/horae-070/libperlxray/Ifeffit/Makefile.PL

cd horae-070
./build


# now build from svn
cd ..
svn co http://cars9.uchicago.edu/svn/horae/trunk horae

cd horae
svn update

perl Build.PL
./Build
./Build install

cd ../local/bin
cp ../../Sources/FixPerlLocal .
sh ./FixPerlLocal


