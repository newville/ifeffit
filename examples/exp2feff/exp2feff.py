#!/usr/bin/python
#
# converts experimental chi(q) data into a feffNNNN.dat 
# file for use as an 'experimental standard'
# 
# 
import sys, getopt, string

doc = """
 exp2feff
 Syntax: exp2feff datafile

 You will be prompted for information about 
 the input file colums, an approximate R, 
 the k-weighting used to make this data,
 and the name of an output file

"""

def show_usage():
    print doc
    sys.exit(1)

try:
    import Ifeffit
    iff = Ifeffit.Ifeffit()
    iff.ifeffit(" " )
except:
    print "Cannot import Ifeffit python extension"
    sys.exit(1)


# get file
if  (len(sys.argv) > 1):
    file  = sys.argv[1]
else:
    file = raw_input(" Enter filename for filtered chi(k) data:")

try:
    f = open(file,'r')
    f.close()
except:
    print "Error: cannot open file ", file



iff.ifeffit(" read_data(file=%s, group=c)"% (file))
lab = iff.get_string('column_label')

print " Columns read from file:  ", file
print "  ' %s '" % lab

(ik, im, ip) = (-1,-1,-1)

print """
  Please enter column **NUMBERS** for the columns containing
     k, |chi(k)|, and phase[chi(k)],
  separated by spaces. For example: 1 2 3"""


s  = raw_input(' Column numbers for k, amp, phase [1 2 3]')
if (len(s) < 4): s = '1 2 3'
try:
    s1 = string.split(s)
    (ik, im, ip) = (int(s1[0]), int(s1[1]), int(s1[2]))
except:
    print "Error: can't read column numbers from: ", s
     
    

print "   using columns ", ik, im, ip , " for k, amp, phase"

iff.ifeffit(" read_data(file=%s, group=c, type=raw)"% (file))
iff.ifeffit(" set c.k    = c.%i"% (ik))
iff.ifeffit(" set c.xmag = c.%i"% (im))
iff.ifeffit(" set c.xpha = c.%i"% (ip))


kk = [.000 , .100 , .200 , .300 , .400 , .500 , .600 , .700 , .800 , .900 ,
   1.000 , 1.100 , 1.200 , 1.300 , 1.400 , 1.500 , 1.600 , 1.700 , 1.800 ,
   1.900 , 2.000 , 2.200 , 2.400 , 2.600 , 2.800 , 3.000 , 3.200 , 3.400 ,
   3.600 , 3.800 , 4.000 , 4.200 , 4.400 , 4.600 , 4.800 , 5.000 , 5.200 ,
   5.400 , 5.600 , 5.800 , 6.000 , 6.500 , 7.000 , 7.500 , 8.000 , 8.500 ,
   9.000 , 9.500 , 10.000 , 11.000 , 12.000 , 13.000 , 14.000 , 15.000 ,
   16.000 , 17.000 , 18.000 , 19.000 , 20.000 ]


s  = raw_input(' Enter k-weight used to make this chi(k) [2]:')
try:
    kw = float(s)
except:
    kw = 2

iff.ifeffit(" set kw = %14.5f" % kw)

s  = raw_input(' Enter Atomic Number for Central Atom [29]:')
try:
    iz0 = int(s)
except:
    iz0 = 29

s  = raw_input(' Enter Atomic Number for Scattering Atom [%i]:' % iz0)
try:
    iz1 = int(s)
except:
    iz1 = iz0

s  = raw_input(' Enter approximate first shell R [2.5]:')
try:
    r1 = float(s)
except:
    r1 = 2.5
    

iff.ifeffit(" set r1 = %14.5f" % r1)


iff.ifeffit(" set c.pha = c.xpha - 2*r1*c.k")
iff.ifeffit(" set tiny  = 1.e-6")
iff.ifeffit(" set c.mag = 0.5*c.xmag/(c.k+tiny)**(kw-1)")

outf = raw_input(' Enter name of output file [feff_exp.dat]:')
if (len(outf) < 1): outf = 'feff_exp.dat'



o = open(outf, 'w')

header=""" dummy Feff.dat file by exp2feff                                 Feff XX 6.10

 ALL FAKE: r = %7.4f,  kw = %7.4f 
 Gam_ch=1.761E+00 H-L exch
 Mu=-5.000+00 kf=2.000E+00 Vint=-0.000E+01 Rs_int= 2.000
 Path    1      icalc       2
 -----------------------------------------------------------------------
   2  1.000  %7.4f   2.000   -5.0 nleg, deg, reff, rnrmav(bohr), edge
      x         y         z   pot at#
    .0000     .0000     .0000  0  %2i  XX     absorbing atom
    .0000     .0000   %7.4f  1  %2i 
  k   real[2*phc]   mag[feff]  phase[feff] red factor   lambda     real[p]@#
""" % (r1, kw, r1, iz0, r1, iz1)

o.write(header)

iff.put_array("out.k", kk)

iff.ifeffit(" set out.magf = splint(c.k,c.mag,out.k)")
iff.ifeffit(" set out.phaf = splint(c.k,c.pha,out.k)")
iff.ifeffit(" save exp2feff.sav")


k   = iff.get_array("out.k")
mag = iff.get_array("out.magf")
pha = iff.get_array("out.phaf")

n = len(k)
for i in range(len(k)):
    if (i == 0): mag[i] = 0.
    s = "%7.3f %11.4e %11.4e %11.4e %10.3e %11.4e %11.4e\n" % (k[i],0.0,mag[i],
                                                             pha[i],1.0,1.e8,k[i])
    o.write(s)

o.close()
print "out = ", outf
