#  -*-python-*-
#  Python site installation information
#  This file was auto-generated during the Ifeffit build
#  and may need some modifications.
#  
bin_dir="/usr/local/bin"
IFEFFIT_DIR="/usr/local/share/ifeffit"
IFEFFIT_BIN="/usr/local/share/ifeffit"
PGPLOT_DIR="/usr/local/share/ifeffit/pgplot"
PGPLOT_DEV="/XS"
#  
import os
if os.name == "nt":
    PGPLOT_DEV="/GW"
    IFEFFIT_DIR="C://Program Files//Ifeffit"
    IFEFFIT_BIN="C://Program Files//Ifeffit//bin"
    PGPLOT_BIN ="C://Program Files//Ifeffit//bin"
    PGPLOT_DIR ="C://Program Files//Ifeffit//bin"
    bin_dir = "Scripts"
