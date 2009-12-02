import os
PGPLOT_DEV='/XS'
IFEFFIT_DIR='/usr/local/share/ifeffit'
IFEFFIT_BIN='/usr/local/share/ifeffit'
PGPLOT_DIR='/usr/local/share/ifeffit/pgplot'
bin_dir = '/usr/local/bin'

if os.name == 'nt':
    PGPLOT_DEV='/GW'
    IFEFFIT_DIR='C://Program Files//Ifeffit'
    IFEFFIT_BIN='C://Program Files//Ifeffit//bin'
    PGPLOT_BIN ='C://Program Files//Ifeffit//bin'
    PGPLOT_DIR ='C://Program Files//Ifeffit//bin'
    bin_dir = IFEFFIT_BIN

