#!/usr/bin/env python
# To use:
#       python setup.py install
#

import os, sys, string
import distutils
from distutils.core import setup, Extension

try:
    setup_arg = sys.argv[1]
except:
    setup_arg = ''    

vx = sys.version_info

if vx[0] < 2: 
    print "Error: python is too old"
    sys.exit(1)

version = '2.0.0'
#
# include and parse the data from site_install
from  lib.ifeffit_config import bin_dir
    
setup (name             = "Ifeffit",
       version = '2.0.0',
       maintainer       = "Matt Newville",
       maintainer_email = "newville@cars.uchicago.edu",
       description      = "Ifeffit Extension to Python",
       url              = "http://cars9.uchicago.edu/ifeffit",
       package_dir      = {'Ifeffit': 'lib'},
       packages         = ['Ifeffit'],
       data_files       = [(bin_dir, ['ifeffit_shell.py'])])

