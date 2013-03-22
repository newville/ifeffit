#!/bin/sh
# set environment for using and building with Ifeffit / iXAFS.app codes

. local/bin/iff_init.sh
#DEVEL:
arch='-arch i386 -arch ppc'
arch='-arch i386'
export F77=gfortran
export LDFLAGS="-L$local/lib"
export FLIBS="-L$local/lib -lgfortran -lgfortranbegin"
export CFLAGS="-O2 $arch -mmacosx-version-min=10.5 -isysroot/Developer/SDKs/MacOSX10.5.sdk"
export CXXFLAGS="-O2 $arch -mmacosx-version-min=10.5 -isysroot/Developer/SDKs/MacOSX10.5.sdk"
export FFLAGS="-O2 $arch -mmacosx-version-min=10.5 -isysroot/Developer/SDKs/MacOSX10.5.sdk"
export ARCHFLAGS="-O2 $arch"
export PERL5LIB="$local/lib/perl5/site_perl:$local/lib/perl5/site_perl/5.12.1/darwin-thread-multi-2level"
export TOP=`dirname $local`
