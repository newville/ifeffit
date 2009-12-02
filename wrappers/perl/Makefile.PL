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
                    ' -lifeffit -L/usr/local/share/ifeffit/pgplot -lpgplot_iff -lpng -lz -lX11 '.
                    join(" ", (sort(split(" ", "-L/usr/lib/gcc/i586-redhat-linux/4.4.1 -L/usr/lib/gcc/i586-redhat-linux/4.4.1/../../.. -lgfortranbegin -lgfortran -lm -lgcc_s" ))))] 
         );
