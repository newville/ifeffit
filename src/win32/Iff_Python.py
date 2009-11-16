import ctypes

##libiff = ctypes.windll.LoadLibrary('ifeffit_12.dll')
libiff = ctypes.cdll.LoadLibrary('./ifeffit.so')

# iff_exec = getattr(libiff,'iff_exec@4')
iff_exec = libiff.iff_exec

iff_exec('my.x = range(1,12,1)')
iff_exec('my.y = sin(my.x/13) + cos(my.x/7)')
iff_exec('show $&build')
# iff_exec('plot my.x, my.y, color=red')
# print "How cool is that??"
iff_exec('x = 22.1')

pd = ctypes.pointer(ctypes.c_double())
libiff.iff_get_scalar('x',pd)

print pd.contents.value


pd = (200*ctypes.c_double)()
print pd

n = libiff.iff_get_array('my.y',pd)
print n
print [i for i in pd[:n]]

for i in range(n):
    pd[i] = -pd[i]/30.0

pn = ctypes.pointer(ctypes.c_int(n))
n = libiff.iff_put_array('my.z',pn,pd)

iff_exec('show @arrays')
iff_exec('print my.z')
