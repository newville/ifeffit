#
# target program that links to so
EXE     = if_test
OBJ     = if_test.o 
LIBS    = -L/usr/local/lib -lifeffit -L/usr/local/pgplot -lpgplot -L/usr/X11/lib -lX11 -lg2c -lm
CC      = gcc -O2
CLIBS   = -lc -lm
INCS    = -I/usr/local/include

$(EXE): $(OBJ) 
	$(CC)  -o $(EXE) $(OBJ) $(INCS) $(LIBS) $(CLIBS) 


.PHONY: clean
clean :
	rm $(OBJ) $(EXE)


