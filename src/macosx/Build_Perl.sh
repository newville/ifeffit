#!/bin/sh

## Build and Install Perl to /Applications/iXAFS.app

. devel_init.sh

BUILD=$TOP/build/PL

mkdir -p $BUILD

## First, using MacPorts, which uses /opt/local as its default install location.
## I did
#
# export PATH=/opt/local/bin:/opt/local/sbin:$PATH

# /opt/local/bin/port -vk configure perl5.12 +mangle_names +universal

## This patched and built perl, but failed at installation due to permision to
## write to /opt/local.  That's fine, we don't want to use MacPorts
## anyway... but we'll happily use their patches!

# This is where MacPorts installed perl 5.12
# MACPORT_PERL=$HOME/.macports/opt/local/var/macports/sources/rsync.macports.org/release/ports/lang/perl5.12/work/perl-5.12.1

## forget about Macports:
# export PATH=$PATHSAVE
cd $TOP/build/PL
cp -pr $MACPORT_PERL  .

cd $BUILD

tar xvzf ../../Sources/Perl-5.12_MacPort.tar.gz
cd perl-5.12.1

mv config.sh config_Macports.sh
sed "s|/opt/local|$TOP/local|g" config_Macports.sh > Tmp
sed "s|-arch i386|-arch i386 -arch ppc|g" Tmp   > config.sh
./Configure -desr
make
make install

sed "s|_TOP_|$TOP|g" $TOP/Sources/CPAN_Config.pm > $TOP/local/lib/perl5/5.12.1/CPAN/Config.pm

cd $TOP/build

Perllibs='Regexp-Common-2010010201 Tk-804.209'

for f in $Perllibs; do
  tar xvzf ../Sources/$f.tar.gz
  cd $f
  perl Makefile.PL
  make install
done

cd ..

mkdir -p Bundle

cp -pr ../Sources/HoraeBundle.pm Bundle/.

perl -MCPAN -e 'install Bundle::HoraeBundle'



