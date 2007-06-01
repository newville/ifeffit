#  
#  Tcl Setup.in auto-generated during Ifeffit build
#  This file may need some modifications, but should give
#  a good guess for how to build the Ifeffit extension.
#  
#==  Ifeffit build configuration:
#    LIB_IFF = ifeffit library  
#    LIB_PLT = PGPLOT  libraries
#    LIB_F77 = Fortran libraries
#    LIB_X11 = X Libaries
#    INC_IFF = location of ifeffit.h
LIB_IFF  = -L/usr/local/lib -lifeffit 
LIB_PLT  = -L/usr/local/pgplot -lpgplot -L/usr/X11R6/lib -lX11 
LIB_F77  = -lg2c -lm -L/usr/lib/gcc-lib/i386-redhat-linux/2.96 -L/usr/lib/gcc-lib/i386-redhat-linux/2.96/../../.. -lm 
INC_IFF  = -I/usr/local/include 
#==  
LIB_IFF  = -L../../src/lib -lifeffit 
INC_IFF  = -I../../src/lib 
CC       = gcc
CFLAGS   = -g -O2
LDFLAGS  = 
#
# Makefile for the Ifeffit extension to tcl
# location of Tcl/Tk libraries
LIB_TK    = -L/usr/lib -ltcl -ltk
# location of tcl
TCL       = tcl

#
# how to compile C code to an object file for dynamic loading
SHARE_FLAG = -fpic
COMPILE   = $(CC) $(SHARE_FLAG) $(CFLAGS) -c -I../../src/lib
LD        = $(CC) -shared 

LIBS      =  $(LIB_IFF) $(LIB_PLT) $(LIB_F77) $(LIB_TK) $(LIB_X11)
default:  ifeffit_tcl.so

.PHONY:  clean

ifeffit_tcl.so: ifeffit_wrap.o
	$(LD) ifeffit_wrap.o $(LIBS) -o ifeffit_tcl.so

ifeffit_wrap.o: ifeffit_wrap.c
	$(COMPILE) ifeffit_wrap.c -o ifeffit_wrap.o

test: ifeffit_tcl.so
	$(TCL) test.tcl

clean:
	rm ifeffit_wrap.o ifeffit_tcl.so
