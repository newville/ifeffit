#!/usr/bin/perl -w
use lib '.';
$| = 1;
print "===  Testing Ifeffit Perl Module  ===\n";

$test = 0; $pass = 0;
#----------------------------------------------------------------
# load ifeffit 
$test++;
print "==============\n <$test>: loading Ifeffit .. ";
eval "use Ifeffit";  
if ($@ ne "") {print "FAILED\n"; die;}
 
use Ifeffit qw(get_scalar get_string get_array);
use Ifeffit qw(put_scalar put_string put_array);
use Ifeffit qw(get_echo);


$pass++;
#----------------------------------------------------------------
# call ifeffit 
$test++;

print "==============\n <$test>: simple call to ifeffit:\t ifeffit(\"x = 1.23450\");\n";
$i = ifeffit( "x = 1.234500");
if ($i eq 0) { 
    print " Looks good. Let's look at the result with  ifeffit(\"show x\") \n";
    print " you should see \"x   =   1.2345\" on the line below ...\n";
    ifeffit( "show x");
} else {
    print "\tLooks bad.\n";
}

$pass++;
#----------------------------------------------------------------
# here doc
$test++;

print "==============\n <$test>: send  a perl 'here-document' to ifeffit:\n";

# --------
my ($file, $type, $kweight)  = ("a.xmu", "xmu", 1);
my $read_data =<<"END";
              set (kmin = 0., kweight = $kweight, rbkg = 1.2)  
              read_data(file = $file,  prefix = my, type = $type) 
END
print "$read_data";

$i = ifeffit($read_data);

if ($i eq 0) { 
    print "  ifeffit read xmu data and has <my. energy> and <my.xmu> arrays:\n";
    ifeffit(" show  my.energy, my.xmu"); 
} else {
    print "\tLooks bad.\n";
}

$pass++;
#----------------------------------------------------------------
# get scalar
$test++;

print "==============\n <$test>: get_scalar:\n";
print "  \t    \$x = get_scalar(\"x\");\n";
my $x = get_scalar("x"); 
print "      x  = $x\n";


$pass++;
#----------------------------------------------------------------
# put scalar
$test++;

print "==============\n <$test>: put_scalar:\n";
print "  \t  \$phi = put_scalar(\"phi\", 1.605);\n";

my $phi = put_scalar("phi", 1.605);

print "   gives  \$phi = $phi , and ifeffit(\"print phi\") shows:\n";
ifeffit("print phi");


$pass++;
#----------------------------------------------------------------
# put string
$test++;

print "==============\n <$test>: use perl's put_string\n";
print "  \t  put_string(\"\$filename\", \"my.xmu\")\n";

put_string("\$filename", "my.xmu");
print "  \t  ifeffit (\" show \$filename\"):\n";
ifeffit (" show \$filename");

$pass++;
#----------------------------------------------------------------
# get string
$test++;

print "==============\n <$test>: get_string\n";
print "  get_string('filename')  returns: ". get_string('filename')."\n";
print "  get_string('&build')    returns: ". get_string('&build')."\n";
 
$pass++;
#----------------------------------------------------------------
# put array
$test++;

print "==============\n <$test>: put_array\n";
print "  an array made within perl :\n";
for ($i = 0; $i< 5; $i++ ) { $z[$i] = sin($i * 799 + 99.111);}
print  "[ ". join(", ",@z), "]\n  was sent to Ifeffit array my.test:\n";
$i =  put_array("my.test", \@z);
 
ifeffit("show my.test"); 

$pass++;
#----------------------------------------------------------------
# get array
$test++;

print "==============\n <$test>: get_array('my.test')\n";

@e =  get_array("my.test");
print " result: [ ". join(", ",@e), "]\n";

print "==============\n  Tests Done\n";

$pass++;
#----------------------------------------------------------------
# get echo
$test++;
print "==============\n <$test>: get_echo: \n";

ifeffit( "&screen_echo = 0");
ifeffit( "show \@scalars");
ifeffit( "show \@strings");
$n_echo = get_scalar("&echo_lines");
print " now doing get_echo (from 'show \@scalars': $n_echo lines)\n";

for ($i = 0; $i<$n_echo ; $i++) {
    $str = get_echo();
    print " $str \n";
}

print "==== simple print of array, using get_echo to cache results:\n";
ifeffit( "print  indarr(30)");
my $n_echo = get_scalar("&echo_lines");
for ($i = 0; $i<$n_echo ; $i++) {
    $str = get_echo();
    print "::> $str \n";
}
$pass++;


END {
    print "==============\n  Tests Done: ";
    print " Passed $pass / $test tests ... ";
    if ($pass == $test) {
	print " Looks good!\n";
    } else {
	print " Uh oh!\n";
    }
}

