

import ctypes
import types
import cmd
import os
import sys

# from string import strip,find, split

__version__  = '2.02'

path_sep = ':.:'
loadlib = ctypes.cdll.LoadLibrary
dll_name = 'libifeffit.so'

if os.name == 'nt':
    path_sep = ';.;'
    loadlib = ctypes.oledll.LoadLibrary
    dll_name = 'ifeffit_12.dll'
try:
    from config import conf
except:
    conf  = {'PGPLOT_DEV': '/XS',
             'IFEFFIT_DIR': '/usr/local/share/ifeffit',
             'IFEFFIT_BIN': '/usr/local/share/ifeffit',
             'PGPLOT_DIR': '/usr/local/share/ifeffit/pgplot',
             }

oskeys  = os.environ.keys()

for k in conf:
    if k not in oskeys:
        os.environ[k] =  conf[k]

os.environ['PATH']  = os.environ['IFEFFIT_DIR'] + path_sep + os.environ['PATH'] 

try:
    libiff = loadlib(dll_name)
    print 'Load Lib: ', dll_name, libiff
    
except:
    print "Cannot load Ifeffit Module -- check path and installation"
    sys.exit(1)

iff_exec = libiff.iff_exec
iff_exec('')

iff_exec('my.x = range(1,100,1)')
iff_exec('my.y = sin(my.x/13) + cos(my.x/7)')
iff_exec('show $&build')
iff_exec('plot my.x, my.y, color=red')

