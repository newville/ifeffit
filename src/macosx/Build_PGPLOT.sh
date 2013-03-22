!/bin/sh

. devel_init.sh

BUILD=$TOP/build/PG

mkdir -p $BUILD/build
mkdir -p $BUILD/pgplot/sys_macosx
mkdir -p $TOP/local/share/ifeffit/pgplot

cd $BUILD/
tar xvzf ../../Sources/pgplot5.2.tar.gz
cd $TOP

# install mods to PGPLOT
cp -pr $TOP/Sources/pgplot_mods/makemake $BUILD/pgplot/.
cp -pr $TOP/Sources/pgplot_mods/aqdriv.m $BUILD/pgplot/drivers/.
cp -pr $TOP/Sources/pgplot_mods/drivers.list  $BUILD/build/.
cp -pr $TOP/Sources/pgplot_mods/include/*.h  $BUILD/build/.

cp -pr $TOP/Sources/pgplot_mods/grfont/* $TOP/local/share/ifeffit/pgplot/.

sed "s|_TOP_|$TOP|g" $TOP/Sources/pgplot_mods/gfortran_gcc.conf  > $BUILD/pgplot/sys_macosx/gfortran_gcc.conf

cd $BUILD/build
 
../pgplot/makemake ../pgplot macosx gfortran_gcc

sed 's|libpgplot|libpgplot_iff|g' makefile > _Tmp
sed 's|-lpgplot|-lpgplot_iff|g'   _Tmp > _Tmp2
# sed 's|-lpng|-lpng_iff|g'   _Tmp2 > makefile

echo ' done'
make  # libpgplot_iff.a grfont.dat pgxwin_server cpg prog pgplot.doc
make all
cp -pr grfont.dat pgplot.doc makefile rgb.txt drivers.list grexec.f $TOP/local/share/ifeffit/pgplot/.
cp -pr pgxwin_server  $TOP/local/bin/.
cp -pr libpgplot_iff.a  $TOP//local/share/ifeffit/pgplot/libpgplot_iff.a
cp -pr libpgplot_iff.a $TOP/local/lib/libpgplot_iff.a
