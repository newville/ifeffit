#!/usr/bin/python -O 
import Ifeffit

iff = Ifeffit.Ifeffit()
iff.ifeffit("show $&build")
    
# note that a put_scalar can now go first!
t = iff.put_scalar('tt',2.03)

iff.ifeffit('ss = 88.1')
t = iff.get_scalar('ss')

print " ", type(t), " = ", t

iff.ifeffit("$string = \" This is a string\"")
t = iff.get_string("$string")
print " ", type(t), " = ", t

print """
 > ifeffit('my.x = indarr(10)')
 > t = get_array('my.x') """

iff.ifeffit('my.x = indarr(10)')
t = iff.get_array('my.x')
print " t =  ", type(t),  t

print " TEST Put array "
xx = []
for j in  range(len(t)):
    xx.append(t[j] / 23.0)

y  = iff.put_array("my.y", xx)

iff.ifeffit (" show @arrays")
iff.ifeffit (" show @scalars")


print " TEST Delayed echo-ing with get_echo:"
iff.ifeffit(" &screen_echo = 0")
iff.ifeffit(" x = 12.34567890")
iff.ifeffit(" show x")
b = iff.get_echo()


print b
