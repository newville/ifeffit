import ctypes

libiff = ctypes.windll.LoadLibrary('ifeffit_12.dll')

# iff_exec = getattr(libiff,'iff_exec@4')
iff_exec = libiff.iff_exec

iff_exec('my.x = range(1,100,1)')
iff_exec('my.y = sin(my.x/13) + cos(my.x/7)')
iff_exec('show $&build')
iff_exec('plot my.x, my.y, color=red')
print "How cool is that??"
