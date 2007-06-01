#!/usr/bin/python
import Ifeffit
iff = Ifeffit.Ifeffit().ifeffit
iff(" show $&build")


npts = 8192
narr =  128

for p in 'abcdef':
    print "= group %s " % p
    for i in range(narr):
        s = "%s.%3.3i" % (p,i+1)
        u = iff("set %s = range(1,%i,1)" % (s,npts))

    iff(" show @arrays")
    iff(" show @scalars")
    print " erasing ..."
    iff(" erase @arrays")
