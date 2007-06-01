use lib '.';
use Ifeffit qw(ifeffit get_scalar);

print " pi = ".get_scalar("pi")."\n";


print "-------------\n";
ifeffit("e0 = 7112.0");

print "-------------\n";

ifeffit("  print(' e0 = ' e0 'eV')");

print "-------------\n";


# 
$str = Ifeffit::get_echo(); print " ==>  $str\n";
# $str = Ifeffit::get_echo(); print " ==>  $str\n";
# 
