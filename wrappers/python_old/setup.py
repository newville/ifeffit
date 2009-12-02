#!/usr/bin/env python
# To use:
#       python setup.py install
#
try:
    import os, sys, string, re
except:
    print "Error: python is broken"
    sys.exit(1)

try:
    setup_arg = sys.argv[1]
except:
    setup_arg = ''    

vx = string.split(sys.version)
pyvers = vx[0][0:3]
if (float(pyvers) < 1.5):
    print "Error: python is too old"
    sys.exit(1)

try:
    import distutils
    from distutils.core import setup, Extension
except:
    print "Error: Python distutils not found."
    print " please upgrade python or install the distutils module."    
    sys.exit(1)

python  = sys.executable
pkg_site= sys.prefix
pkg_site= os.path.join(pkg_site, 'lib')
pkg_site= os.path.join(pkg_site, 'python' + pyvers)
pkg_site= os.path.join(pkg_site, 'site-packages')

#
# test Tkinter and Pmw installation

try:
    import Tkinter
    print "Tkinter is installed. "
except ImportError:
    print " "
    print "  =====  WARNING   WARNING   WARNING   ====="
    print "  Tkinter is NOT installed, or was not found"
    print "  G.I.FEFFIT will not work."
    print " "

#PMW
installPMW = 1
try:
    import Pmw
    version = Pmw._version
    if  (version == '1.2') :
        installPMW = 0
        print "Pmw %s is installed. " % version
except ImportError:
    pass

if ((installPMW == 1) and (setup_arg == 'install')):
    pmw_dir   = os.path.join(pkg_site, 'Pmw')
    print " Installing Pmw 1.2  to %s " % pmw_dir
    os.system('gunzip -c Pmw.1.2.tar.gz | tar xvf -')
    os.system("cp -pr Pmw %s " %  pkg_site)
    os.system("cp Pmw.pth %s" %  os.path.join(pkg_site,'Pmw.pth'))
    import compileall
    compileall.compile_dir(pmw_dir)

# remove any old Ifeffit.py installations
if (setup_arg == 'install'):
    old_files = ('ifeffitc.so', 'Ifeffit.py', 'Ifeffit.pyc', 'GIFeffit.py',
                 'GIFeffit.pyc', 'TkIfeffit.py', 'TkIfeffit.pyc')
    for i in old_files:
        os.system("rm -f %s/%s" % (pkg_site,i))



version = '1.0c'
#
# include and parse the data from site_install
from site_install import *
def key_parse(s,k):
    import string
    words = string.split(s)
    p = {}
    for i in k:  p[i] = []
    for i in words:
        for k in p.keys():
            lk = len(k)
            if ((i[0:lk] == k) and (i[lk:] not in p[k])): p[k].append(i[lk:])
    return p

libdat  = key_parse("%s %s %s"  % (lib_iff,lib_plt,lib_f77), ['-l','-L'])
incdat  = key_parse(inc_iff,['-I'])
#
src     = ['ifeffit_wrap.c']
ex_link = ''

if os.uname()[0] == 'Darwin':
    ex_link = ['-framework','Foundation']
    for dir in ('/usr/lib','/sw/lib','/Applications/Ifeffit/lib'):
        if (dir not in libdat['-L']): libdat['-L'].append(dir)
    
setup (name             = "Ifeffit",
       maintainer       = "Matt Newville",
       maintainer_email = "newville@cars.uchicago.edu",
       description      = "Ifeffit Extension to Python",
       url              = "http://cars9.uchicago.edu/ifeffit",
       extra_path       = 'Ifeffit',
       version          = version,
       include_dirs     = incdat['-I'],
       ext_modules      = [Extension('_ifeffit', src,
                                     extra_link_args = ex_link,
                                     libraries    = libdat['-l'],
                                     library_dirs = libdat['-L'])],
       py_modules       = ['Ifeffit','TkIfeffit','DataPlotter','GIFeffit'],
       data_files       = [(bin_dir, ['gifeffit'])]  )

try:
    f = open('iff_py_made','w')
    f.write("")
    f.close()
except:
    pass

