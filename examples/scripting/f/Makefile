##
## Fortran Makefile
##
## 1: replace with the contents of Config.mak from
##    /usr/local/share/ifeffit/config

#==  Ifeffit build configuration:
#    LIB_IFF = ifeffit library  
#    LIB_PLT = PGPLOT  libraries
#    LIB_F77 = Fortran libraries
#    LIB_X11 = X Libaries
#    INC_IFF = location of ifeffit.h
LIB_IFF  = -L/usr/local/lib -lifeffit 
LIB_PLT  = -L/usr/local/pgplot -lpgplot 
LIB_F77  = -lg2c -lm -L/usr/lib/gcc-lib/i386-redhat-linux/2.96 -L/usr/lib/gcc-lib/i386-redhat-linux/2.96/../../.. -lm 
LIB_X11  = -L/usr/X11R6/lib -lX11
INC_IFF  = -I/usr/local/include 
#==  


F77       = f77 -O1
LOAD      = $(F77)

TARGET = f1
SRC    = f1.f
OBJ    = f1.o

default: $(TARGET)


.f.o:
	$(F77) -c $*.f

ALL_LIBS = $(LIB_IFF) $(LIB_PLT) $(LIB_X11) $(LIB_F77) 

$(TARGET): $(OBJ)
	$(LOAD) $(OBJ) -o $(TARGET) $(ALL_LIBS)

clean :
	rm -f $(OBJ) $(TARGET)

