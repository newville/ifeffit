
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
