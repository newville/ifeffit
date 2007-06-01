#  -*-perl-*-
#  Perl Makefile.PL auto-generated during Ifeffit build
#  This file may need some modifications, but should give
#  a good guess for how to build the Ifeffit extension.
#  
use lib '.';
use ExtUtils::MakeMaker;
use Config;
WriteMakefile('NAME'   => 'Ifeffit',
              'SKIP'   => ['test_dynamic'],
              'OBJECT' => 'ifeffit_wrap.o',
              'INSTALLBIN'    => '/usr/local/bin',
              'INSTALLSCRIPT' => '/usr/local/bin',
              'LDDLFLAGS'     => '-shared',
              'INC'    => '-I../../src/lib -I/usr/local/share/ifeffit/config',
              'LIBS'   =>[' -L../../src/lib '.$Config{ldflags}.
                    ' -lifeffit -L/usr/local/pgplot -lpgplot -lpng -lz -L/usr/X11R6/lib -lX11 '.
                    join(" ", (sort(split(" ", "-L/usr/lib/gcc-lib/i386-redhat-linux/3.2.2 -L/usr/lib/gcc-lib/i386-redhat-linux/3.2.2/../../.. -lfrtbegin -lg2c -lm -lgcc_s" ))))] 
         );
