CC = gcc
MAKE_DLL=gcc -shared

DLLFLAGS=-Wl,--add-stdcall-alias 
DLLLIBS= -lgfortran -lgcc -lm -mwindows

DLLLIBS= -lgfortran -lgcc -lm -mwindows

PGLIBS=-LGrWin/ -lpgplot -lGrWin
IFFLIB=../lib/libifeffit.a

all: iff_shell.exe ifftest.exe

iff_shell.exe: iff_shell.o ifeffit_12.dll
	$(CC) -o iff_shell.exe iff_shell.o  ifeffit_12.dll

ifftest.exe: ifftest.o ifeffit_12.dll
	$(CC) -o ifftest.exe ifftest.o  ifeffit_12.dll

ifeffit.o: ifeffitc.c ifeffit.h
iff_shell.o: iff_shell.c ifeffit.h
ifftest.o:  ifftest.c ifeffit.h


ifeffit_12.dll: $(IFFLIB) ifeffitc.o
	$(MAKE_DLL) -o ifeffit_12.dll ifeffitc.o $(DLLFLAGS) $(IFFLIB)  $(PGLIBS) $(DLLLIBS)
