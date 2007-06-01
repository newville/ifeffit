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
                    ' -lifeffit -L/usr/local/pgplot -lpgplot -lpng -lz -lX11 '.
                    join(" ", (sort(split(" ", "-L/usr/lib64 -L/usr/lib/gcc-lib/x86_64-redhat-linux/3.2.3 -L/usr/lib/gcc-lib/x86_64-redhat-linux/3.2.3/../../../../lib64 -L/lib/../lib64 -L/usr/lib/../lib64 -lfrtbegin -lg2c -lm -lgcc_s" ))))] 
         );
