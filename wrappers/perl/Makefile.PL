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
              'INSTALLBIN'    => '/usr/bin',
              'INSTALLSCRIPT' => '/usr/bin',
              'LDDLFLAGS'     => '-shared',
              'INC'    => '-I../../src/lib -I/usr/share/ifeffit/config',
              'LIBS'   =>[' -L../../src/lib '.$Config{ldflags}.
                    ' -lifeffit ../../src/win32/GrWin/libpgplot.a ../../src/win32/GrWin/libGrWin.a '.
                    join(" ", (sort(split(" ", "-LC:/MinGW/lib/gcc/mingw32/3.4.5 -LC:/MinGW/lib/gcc -LC:/MinGW/mingw32/lib -LC:/MinGW//lib/ -lfrtbegin -lg2c -lmingw32 -lmoldname -lmingwex -lmsvcrt -luser32 -lkernel32 -ladvapi32 -lshell32" ))))] 
         );
