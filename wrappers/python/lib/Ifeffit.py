#!/usr/bin/python

import ctypes
import os

conf ={'IFEFFIT_DIR':'/usr/local/share/ifeffit/',
       'IFEFFIT_BIN':'/usr/local/share/ifeffit/',               
       'PGPLOT_DEV':'/XS',
       'PGPLOT_DIR':'/usr/local/share/ifeffit/pgplot',
       }

try:
    import numpy
    has_numpy = True
except:
    has_numpy = False

import ifeffit_config as iffconf
for k in conf:
    if k in os.environ:
        conf[k] = os.environ[k]
    elif hasattr(iffconf, k):
        conf[k] = getattr(iffconf, k)


def find_windll(basename, dllname):
    """ search typical install directories for where Ifeffit might be installed"""
    fulldll =  os.path.join(basename,'bin',dllname)
    if os.path.exists(fulldll):
        return basename    
    while not os.path.exists(fulldll):
        for drive in ('C','D','E','F','G'):
            for inst in ('Program Files','Program Files (x86)', ''):
                basename = os.path.join("%s://%s" % (drive, inst), 'Ifeffit')
                fulldll  = os.path.join(basename,'bin',dllname)
                if os.path.exists(fulldll):
                    return basename
    raise ImportError("Cannot find Ifeffit dll")


def set_environ():
    load_dll = ctypes.cdll.LoadLibrary
    
    dllname  = 'libifeffit.so'
    if os.uname()[0].lower().startswith('darwin'):
        dllname  = 'libifeffit.dylib'

    path_sep = ':'
    os_path  = os.environ['PATH']

    if os.name == 'nt':
        load_dll = ctypes.windll.LoadLibrary
        dllname  = 'ifeffit_12.dll'
        path_sep = ';'
        conf['IFEFFIT_DIR'] = find_windll(conf['IFEFFIT_DIR'], dllname)
        for k in ('PGPLOT_DIR', 'IFEFFIT_BIN'):
            os.environ[k] = conf[k]  = os.path.join(conf['IFEFFIT_DIR'], 'bin')
            
        dllname = os.path.join(conf['IFEFFIT_BIN'], dllname)

    if 'PGPLOT_DIR' not in os.environ:
        os.environ['PGPLOT_DIR'] = os.path.join(conf['IFEFFIT_DIR'], 'pgplot')
    if 'PGPLOT_FONT' not in os.environ:
        os.environ['PGPLOT_FONT'] = os.path.join(conf['PGPLOT_DIR'], 'grfont.dat')

    os.environ['PATH'] = "%s%s%s" % (conf['IFEFFIT_BIN'], path_sep, os_path)

    return load_dll, dllname


class Ifeffit():
    def __init__(self,screen_echo=1, use_numpy=True):
        load_dll,dllname = set_environ()
        try:
            self.libiff   = load_dll(dllname)
        except:
            print " failed to load ifeffit library ", dllname
            print os.environ['PATH']
            raise ImportError('Cannot load ifeffit library')
        
        startup_string = "&screen_echo=%i" % (screen_echo)
        self.ifeffit(startup_string)
        self.MAX_ARRAY_PTS = int(self.get_scalar("&maxpts"))
        self.num_array = None
        self.use_numpy = (has_numpy and use_numpy)            
                
    def show_numeric(self):
        return self.num_array is not None

    def ifeffit(self, cmd):        
        coms = cmd.split('\012')
        ret  = 0
        for c in coms:
            if (c != ''):
                ret  = self.libiff.iff_exec(c)
        return ret

    def iff_exec(self, cmd):
        self.ifeffit(cmd)
        
    def __call__(self,cmd):
        return self.ifeffit(cmd)

    def get_scalar(self,name):
        # get_scalar.restype = ctypes.c_int
        pd = ctypes.pointer(ctypes.c_double())
        i  = self.libiff.iff_get_scalar(name, pd)
        return pd.contents.value

    def put_scalar(self,name,val):
        return self.ifeffit("%s = %s" % (name, val))
        
    def get_array(self,name):
        pdat = (self.MAX_ARRAY_PTS*ctypes.c_double)()
        nout = self.libiff.iff_get_array(name, pdat)
        arr = [i for i in pdat[:nout]]
        if self.use_numpy:  arr = numpy.array(arr)
        return arr

    def put_array(self,name,value):
        n  = len(value)
        pn = ctypes.pointer(ctypes.c_int(n))

        pdat = (self.MAX_ARRAY_PTS*ctypes.c_double)()
        for i in range(n):
            pdat[i] = value[i]
        n = self.libiff.iff_put_array(name,pn,pdat)

    def get_string(self,name):
        "return an Ifeffit string as a python string"
        sout = " "*512
        self.libiff.iff_get_string(name, sout)
        return sout.strip().rstrip()

    def put_string(self,name):
        "return an Ifeffit string as a python string"
        if not name.startswith('$'):
            name = '$%s' % name
        return self.ifeffit("%s = %s" % (name,val))

    def get_echo(self):
        "get lines from Ifeffit's echo cache."
        str = " "*512
        ret = self.libiff.iff_get_echo(str)
        return str.strip().rstrip()

    def get_echo_buffer(self):
        "return the full echo buffer"
        s = []
        n = int(self.get_scalar('&echo_lines'))
        for i in range(n):
            s.append(self.get_echo())
        return s

    def echo_off(self):
        self.echo_orig =  int(self.get_scalar("&screen_echo"))
        self.ifeffit("set &screen_echo=0")

    def echo_on(self):
        self.ifeffit("set &screen_echo=%i"%self.echo_orig)
        self.echo_orig = 0


    def clear_echo_buffer(self):
        "make sure IFeffit's echo buffer is clean"
        tmp = self.get_echo_buffer()
        return None

    def clear_arrays(self):
        m = self.list_arrays()
        for i in m:
            self.ifeffit("erase %s" % i)
            
    def list_arrays(self):
        self.get_echo_buffer()
        self.echo_off()

        self.ifeffit("show @arrays")
        s = self.get_echo_buffer()
        x = []
        for i in s:  x.append(i.split('=')[0].strip().rstrip())
        self.echo_on()
        return x

    def read_file(self,file=None):
        "return data structure for arrays read from a file"
        if (file == None): return None
        self.ifeffit("read_data(%s, group=t)" % (file))
        colnames = self.get_string("column_label").split()
        out = {}
        for i in colnames:
            out[i] = self.get_array("t.%s" % (i))
        return out,colnames

    def get_group_arrays(self):
        "return dictionary of ifeffit groups/aarrays"
        self.clear_echo_buffer()
        dict = {}
        self.echo_off()
        self.ifeffit("show @arrays");
        buff= self.get_echo_buffer()
        self.echo_on()
        for s in buff:
            if len(s)>1 and s.find('=') >1:
                sx   = s.split('=')
                npts = 0
                if (len(sx) < 0):  sx = ['','']
                npts = sx[1].split('pts')[0].strip()
                name = sx[0]
                print name, npts
                try:
                    pre,suff  = name.split('.')
                    if pre not in dict:
                        dict[pre] = {'names':[], 'data':[]}
                    dict[pre]['names'].append(suff.strip())
                    dict[pre]['data'].append(self.get_array(name))
                except ValueError:
                    pass
        return dict

    def reset_plot_opts(self):
        " set generic plotting options into structure"
        p   = {'bg': 'white', 'fg': 'black',
               'gridcolor': '#CCBEE0',
               'showgrid' : 1,   'charsize' : 1.5,
               'charfont' : 1,   'linewidth': 2,
               'linestyle': 1,
               'title': "",
               'xlabel': "",  'ylabel': "",
               'xmin': "",    'xmax': "",
               'ymin': "",    'ymax': "",
               'traces': [['blue',      'solid'],
                          ['blue',      'solid'],
                          ['red',       'solid'],
                          ['black',     'solid'],
                          ['darkgreen', 'solid'],
                          ['magenta',   'solid'],
                          ['maroon',    'solid'],
                          ['yellow',    'solid'],
                          ['orange',    'solid'],
                          ['purple',    'solid']]
               }
        return p


    def get_plot_opts(self):
        " get plotting options from current ifeffit session into structure"
        p   = self.reset_plot_opts()
        # print "getting plot opts"
        self.clear_echo_buffer()
        self.ifeffit('color("show")')
        nt = len(p['traces'])
        x  = self.get_echo()
        for i in range(self.get_scalar('&echo_lines')):
            x = self.get_echo()
            k,v = [i.strip() for i in x.split('=')]
            if (k == 'bg'):
                p['bg'] = v
            elif (k == 'fg'):
                p['fg'] = v
            elif (k == 'grid'):
                p['gridcolor'] = v
            else:
                try:
                    j = int(k)
                    if (j < nt):
                        p['traces'][j][0] = v
                    else:
                        p['traces'].append([ v, 'solid'])
                except ValueError:
                    pass
    ##
        self.ifeffit('linestyle("show")')
        n_traces = len(p['traces'])
        x = self.get_echo()
        for i in range(self.get_scalar('&echo_lines')):
            x = strip(self.get_echo())
            k,v = [i.strip() for i in x.split('=')]
            try:
                j = int(k)
                if j < n_traces:
                    p['traces'][j][1] = v
                else:
                    p['traces'].append([ 'black', v])
            except ValueError:
                pass
        return p

#     def set_plot_opts(self,p):
#         " set plotting to ifeffit session from plot structure"
#         self.ifeffit ("color(fg='%s', bg='%s', gridcolor='%s')"%
#                       (p['fg'], p['bg'], p['gridcolor']) )
# 
#         for i in range(1,len(p['traces'])):
#             self.ifeffit ("color(%i='%s')"     % ( i, p['traces'][i][0]) )
#             self.ifeffit ("linestyle(%i='%s')" % ( i, p['traces'][i][1]) )
 
if __name__ == '__main__':
    
    iff = Ifeffit()
    print "build:  '%s'" % iff.get_string('$&build')

    ox = iff.ifeffit("read_data(cu10k.chi,group=c)")

    iff.ifeffit('plot c.k, c.chi_k_')
    
    q =iff.get_group_arrays()
    print iff.get_array('c.k')
#     print q.keys()
#     print os.environ['PATH']
#     print os.environ['PGPLOT_DIR']
#     print os.environ['PGPLOT_FONT']
    
# 
# ix = Ifeffit()
# ix.ifeffit('x = 221.3')
# print ix.get_scalar('x')
