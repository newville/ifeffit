#!/usr/bin/perl -w
# Simple shell for ifeffit in perl
use lib '/usr/local/share/ifeffit/perl';
use vars qw($Escape $Prompt $HOME);
use subs qw(ShowHist ShowCommands END HELP IfeffitHelp);
use Getopt::Std;
getopts('xq');

#---------------------------------------------------
#  variables the user may want to customize are
#  initialized before reading .ifeffitrc.
$HOME             = $ENV{HOME};
$RCFile           = "$HOME/.ifeffitrc";
$Escape           = "!";
$Prompt_main      = "Ifeffit>";
$Prompt_more      = "  ...  >";
$history_file     = "$HOME/.ifeffit_hist";
$history_lines    = 500;
@shell_commands   = qw(cd cp mv ls rm mkdir rmdir more cat pwd vi);
@ifeffit_commands = qw(plot newplot spline show fftf fftr load print
		       pre_edge read_data write_data exit quit ff2chi
		       zoom cursor help minimize set macro);
%IF_coms=(
	  "plot"       => "general 2-d plot ",
	  "newplot"    => "general 2-d plot, after clearing plot screen ",
	  "spline"     => "XAFS background removal, ala autobk ",
	  "show"       => "display scalar, array, or text Program Variables ",
	  "print"      => "print value of scalar or array variable or expression",
	  "fftf"       => "forward (k->R) XAFS Fourier transform ",
	  "fftr"       => "reverse (R->k) XAFS Fourier transform ",
	  "load"       => "execute a file of Ifeffit commands ",
	  "pre_edge"   => "determine e0, pre-edge line, and edge jump of xmu data",
	  "read_data"  => "read arrays from data file ",
	  "write_data" => "write arrays and text strings to data file ",
	  "exit"       => "leave Ifeffit ",
	  "quit"       => "leave Ifeffit ",
	  "ff2chi"     => "combine feff paths to give k and chi arrays ",
	  "zoom"       => "pick window on plot screen with mouse ",
	  "cursor"     => "pick x-y coordinates on plot screen with mouse ",
	  "minimize"   => "generalized least-squares fit ",
	  "set"        => "define a scalar, array, or text Program Variable ",
	  "macro"      => "define an ifeffit macro");

#---------------------------------------------------
# load needed perl modules
$,=" ";  $|=1 ;


#---------------------------------------------------
# load needed perl modules
$,=" ";  $|=1 ;

# make sure we can use ifeffit module
print "  Ifeffit";
eval "use Ifeffit"; 
if ($Ifeffit::VERSION) { 
    ifeffit("print \$&build");
    print "  Ifeffit Shell  Perl Module $Ifeffit::VERSION ";
} else {
    die "Perl Module not installed properly\n";
}
# readline module
eval "use Term::ReadLine"; my $does_readline = ($@ eq "");
unless ($opt_q) { 
    print $does_readline ? "(ReadLine enabled) " : "(No ReadLine) " ;
}
print "\n";
#-------------------------------------
# load startup files, then files listed on command line
if ($opt_x) { 
    print "no configuration file loaded \n" unless ($opt_q); 
} elsif ( -e $RCFile ) {
    print "  reading $RCFile ... " unless ($opt_q); 
    eval 'require "$RCFile"';
    unless ($opt_q) {($@ eq "") ? print " OK\n": print "problem\n"; }
}
my $retval = 0;
foreach my $f (@ARGV) {
    print "  loading $f ..."  unless ($opt_q); 
    $retval = ifeffit("load $f\n");
    unless ($opt_q) { ($retval==0) ? print " OK\n" : print " problem!\n";}
}

my $shellcoms = join ("|", @shell_commands ); # used below in regex match
#-------------------------------------
#  set up readline stuff
if ($does_readline) {
    $term = new Term::ReadLine 'ifeffit', \*STDIN, \*STDOUT ;
    $term->ornaments(1);
# read in history from last session
    if ((-e "$history_file")&& (open HIST, "<$history_file")) {
	my @allhist = <HIST>;
	close HIST;
	map s/\n//g , @allhist ;
	foreach (@allhist) {  $term->addhistory($_); }
    }
# set completion model: shell_commands and ifeffit_commands
#                       for 1st word, filenames for rest of line
    my $attribs = $term->Attribs;
    $attribs->{attempted_completion_function} = sub {
	my ($text, $line, $start, $end) = @_;
	if ((substr($line, 0, $start) =~ /^\s*$/) or 
	    (substr($line, 0, $start) =~ /^help/i) ) {
	    $attribs->{completion_entry_function} = $attribs->{'list_completion_function'};
	    $attribs->{completion_word} = [@shell_commands, @ifeffit_commands];
	    return $term->completion_matches($text, $attribs->{'completion_entry_function'});
	} else {
	    $attribs->{completion_entry_function} = $attribs->{'filename_completion_function'};
	    return ();
	}
    }
}
#
# determine prompt and reading mechanism
if ($does_readline) {          # use readline if available
    $getline = sub { my $p = shift; 
		     my $j = $term->readline($p); 
		     $j = "__NULL_\n\n" unless $j;
		     chomp $j; $j =~ s|~|$ENV{"HOME"}| ; return $j; };
} else {                       # or use simple read
    $getline = sub { my $p = shift;
		     print "$p"; my $j = <>; 
		     $j = "__NULL_\n\n" unless $j;
		     chomp $j; $j =~ s|~|$ENV{"HOME"}| ; return $j; };
}
#-------------------------------------

##  main loop
my ($line, $prom, @args);
if ($opt_x) {$line += $opt_x};
while (1) {
    $prom = ($retval == -1) ? $Prompt_more : $Prompt_main; # decide prompt
    $_ = &$getline($prom);         # prompt for line 
    next if ((/^\s*$/) or (/^__NULL/));
    process_command($_) if ($_);
}
## done
#-------------------------------------
sub process_command{
    $line = $_ = shift;
    @args = split();          #
    $_    = lc(shift(@args));      # simplifies the matching below
# execute:                         # based on first word (now in $_)
    exit if (/^quit$/o or /^exit$/o);
    if (/^\?$/o or /^help$/o) { Help($line); }
    elsif (/^\?\?$/o)         { ShowCommands;}
    elsif (/^l$/o)            { ShowHist(@args); }
    elsif (/^cd$/o)           { chdir("@args"); }
    elsif (/^rm$/o)           { unlink("@args"); }
    elsif (/^perl$/o)         { eval("@args"); }
    elsif (/^($shellcoms)$/)  { system("$line"); }
    elsif (/^$Escape/)        { system(substr("$line",1)); }
    else {
	$line =~ s|~|$ENV{"HOME"}| if (/^load$/o or /^read/o or /^write/o); 
	$retval = ifeffit("$line\n");
    }
}
sub ShowHist {  # use 'l' to show command history
    if ($does_readline) {
	my $n = $#_ > -1 ? shift : 10;
	my @h = $term->GetHistory();
	my $min = $#h < $n-1 ? 0 : $#h-$n+1;
	map {print "$_: $h[$_]\n"} ($min..$#h);
    }
}

sub ShowCommands {
    print "Ifeffit is aware of these shell commands:\n ";
    my $i = 0;
    foreach my $t (@shell_commands) {
	 print " $t";
	 $i  += 1 + length($t);
	 if ($i > 60) {print "\n "; $i  = 0;}
     }
    print "\n" unless ($i ==  0);
}

sub Help {
    @args = split(/\s+/, $_[0]);
    if ($#args < 1) {  #    general help from pod
	print "Help is on the way ...\n";
	system ("perldoc $0\n");
    } else {
	shift @args;
	foreach my $t (@args) {IfeffitHelp($t);}
    }
}

sub END {  # executed at 'exit'
    # save history in history file
    if ($does_readline) {
	my @a= $term->GetHistory() if $term->can('GetHistory');
	$#a-- if $a[-1] =~ (/^\s*(quit|exit)\s*$/); # don't save the exit command!
	@a= @a[($#a-$history_lines+1)..($#a)] if $#a > $history_lines;
	if (open HIST, ">$history_file" ) {
	    print HIST join("\n",@a);
	    close HIST;
	} else {
	    print "Can't open $history_file\n";
	}
    }
    print "\tHave a nice day\n" unless ($opt_q); 
}

sub IfeffitHelp {  
    my $t = shift;
    my ($s) = ("unknown command (may be a shell command)");
    print " $t: ";
    foreach my $k  (keys %IF_coms) {
	if ($k eq $t) { $s = $IF_coms{$k};}
    }
    print " $s\n";
}


__END__

=head1 NAME

ifeffit - Shell Interface to IFEFFIT 

=head1 SYNOPSIS

B<ifeffit> is a shell interface to the Ifeffit XAFS Analysis System.
Ifeffit commands are entered and executed as they are typed in, giving
complete access to the Ifeffit library in a relatively friendly
command-line program.

A sample session might look like this:

 %~>ifeffit   
   Ifeffit  1.0004  Copyright (c) 2000 Matt Newville, Univ of Chicago  
   Ifeffit Shell  Perl Module 1.2 (ReadLine enabled) 
   reading /home/newville/.ifeffitrc ... OK
 Ifeffit> read_data(My.xmu,type=xmu)
 Ifeffit> spline(energy,xmu, rbkg = 1.0, kweight =1 )
 Ifeffit> plot(energy, xmu)
 Ifeffit> plot(energy, bkg)

=head1 DESCRIPTION

If you have the Perl module for the ReadLine library (Term::ReadLine and
Term::Readkeys are both required), you can use the command-line editing
features of the Gnu ReadLine library.  Most importantly, this library makes
the up arrow scroll through previous commands.  It also gives you access to
the last 300 commands typed in (stored in the file F<.ifeffit_hist> in your
home directory) in previous sessions.  The command C<l [number]> shows you
the last C<number> commands typed in.

There is also some support for command-line completion via the tab key.
While typing the first word at the command-line prompt, selected ifeffit
commands and shell commands will be 'tab-completed', meaning that hitting
the tab key will complete as much of a partially-typed command as possible.
The list of ifeffit and shell commands recognized for this purpose is
customizable.  After the first word has been fully typed, the tab key will
complete file names in the current working directory.

e.g.:

  %~>ifeffit
    Ifeffit  1.0002  Copyright (c) 2000 Matt Newville, Univ of Chicago  
    Ifeffit Shell  Perl Module 1.2 (ReadLine enabled) 
    reading /home/newville/.ifeffitrc ... OK
  Ifeffit>

=over 4

=item 1.

'quit' and 'exit' will exit the program.

'l' lists the history buffer

'?'  is an alias for help, which will display this document, or give a 
     brief command summary  (as in: 'help plot'). 

'??' lists the known system shell commands that can be executed from
     within ifeffit.  For other shell commands, use the '!' character
     to escape to the system shell.

=item 2.

The resource file F<~/.ifeffitrc> (if found) is read at start-up to allow
customizations.  This file is read as a B<perl> script (not as raw ifeffit
commands).  To allow further confusion (err, customization), you can load
files of ifeffit commands at startup -- I<ifeffit myfile> will load the
file F<myfile> at start-up, as if you typed 'load myfile' at the
command-line.  This loading occurs after the F<~/.ifeffitrc> file is read,
which can provide a convenient way to override default macro definitions.

=item 3.

Shell variables: (can be customized in F<~/.ifeffitrc>)

$Escape  [default = '!']  a line starting with this character is sent
to the shell.

$Prompt  [default = 'Ifeffit> ']  command-line prompt.

$HOME    [default from Environment Variable] users home directory.

=item 4.

A useful procedure for developing and testing ifeffit scripts is

    ifeffit> ! emacs script &
              -- add ifeffit code to script and save the file
    ifeffit> load script

=back

=head1 CUSTOMIZATION

A typical ~/.ifeffitrc file might look like this:

   # 
   # start-up perl for shiffit
     push @shell_commands, qw(emacs grep gunzip diff);
   #
   # pre-load some common ifeffit macros
     ifeffit("load $HOME/.ifeffit_macros "); 

This file is "require'd" by I<ifeffit>.  The "push" line, adds a few more
commands to the default list of shell commands.  The "ifeffit" line loads 
a bunch of pre-defined ifeffit macros.

=head1 AUTHOR

Matthew Newville  --  newville@cars.uchicago.edu

=head1 SEE ALSO

I<Term::Readline> perl module to use the GNU Readline library to improve
reading the command-line and enabling history mechanism.  This module is
available from Comprehensive Perl Archive Network.

I<Ifeffit.pm> perl module to use the Ifeffit XAFS Analysis library from
within perl.  Several applications distributed as part of the Ifeffit
Analysis System, including this program, requires this Ifeffit perl module,

I<Ifeffit Reference Manual> main reference for the commands of the Ifeffit
XAFS Analysis system.


=cut
