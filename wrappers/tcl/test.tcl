#
#  ifeffit interface in tcl
#
#  M Newville  Jun 16, 1999

#######################################################################
source Ifeffit.tcl

puts  " Testing tcl wrapper to ifeffit:"
set test 0
puts " Test <$test>:  Basic Loading Worked"
#

incr test +1
puts " --------------------------------"
puts " Test <$test>:  basic commands"
puts "      ifeffit  \"x = sqrt(12.3)\" "
puts "      ifeffit  \"show x\" "
ifeffit  "x = sqrt(12.3)"
ifeffit  "show x"
#
incr test +1
puts " --------------------------------"
puts " Test <$test>:  get_scalar "
puts "   puts  \[get_scalar \"x\"\]"
puts [get_scalar "x"]
#
incr test +1
puts " --------------------------------"
puts " Test <$test>:  put_scalar "
puts "   put_scalar x 33"
puts "   puts  \[get_scalar x]"
put_scalar x 33
puts [get_scalar x]
#
incr test +1
puts " --------------------------------"
puts " Test <$test>:  put_string "
puts "   put_string  title \"this is a title line\""
puts "   ifeffit \"show \$title\""
put_string  "title" "this is a title line"
ifeffit "show \$title"
#
incr test +1
puts " --------------------------------"
puts " Test <$test>:  get_string "
puts "   puts \[get_string \"title\"\]"
puts [get_string "title"]
#
incr test +1
puts " --------------------------------"
puts " Test <$test>:  put_array "
puts "   set l {1. 3. 5. 9. 22.}"
set l {1. 3. 5. 9.  22.}
puts "   put_array \"my.array\" \$l"
put_array "my.array" $l
puts "   ifeffit \"print my.array\""
ifeffit "print my.array"
#
incr test +1
puts " --------------------------------"
puts " Test <$test>:  get_array "
puts "   puts \[get_array \"my.array\"\]"
puts [get_array "my.array"]

puts " --------------------------------"
puts "  End of Tests"



