use ExtUtils::MakeMaker;


my $platform = 'unix';
SWITCH: {
  ($platform = 'windows'), last SWITCH if (($^O eq 'MSWin32') or ($^O eq 'cygwin'));
  ($platform = 'darwin'),  last SWITCH if (lc($^O) eq 'darwin');
  $platform = 'unix';
};
my ($compile_flags, $pgplot_location, $iffdir);
unless ($platform eq 'windows') {
 {
    ($compile_flags, $pgplot_location, $iffdir) = ("", "", `ifeffit -i`);
    $iffdir =~ s/\s*$//;
    print STDOUT
      "Ifeffit's installations directory is $iffdir\n\t(found by capturing \`ifeffit -i\`)\n";
    open C, "$iffdir/config/Config.mak" or
      die "Could not open $iffdir/config/Config.mak file for reading\n";
    while (<C>) {
      next if (/^\s*\#/);
      chomp;
      ($compile_flags   .= (split(/=/, $_))[1]) if (/^LIB/);
      $compile_flags    .= " ";
      ($pgplot_location .= (split(" ", $_))[2]) if (/^LIB_PLT/);
    };
    print STDOUT "Compilation flags (from $iffdir/config/Config.mak):\n\t$compile_flags\n";
  };
  $pgplot_location =~ s/-L//;
};


WriteMakefile
(
 'NAME'         => 'Ifeffit',
 'VERSION_FROM' => 'lib/Ifeffit.pm',
 'PREREQ_PM'    => {
		    'version'             => '0',
		   },
 'INSTALLDIRS'  => 'site',
 'OBJECT'       => 'ifeffit_wrap.o',
 'LIBS'         => [$compile_flags],
 'EXE_FILES'    => [],
 'PL_FILES'     => {},
);
