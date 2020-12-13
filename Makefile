#IDIR =../include
CC=fsharpc
CFLAGS=--nologo

# ODIR=obj
# LDIR =../lib

# LIBS=-lm

# _DEPS = hellomake.h
# DEPS = $(patsubst %,$(IDIR)/%,$(_DEPS))

# _OBJ = hellomake.o hellofunc.o
# OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))


# $(ODIR)/%.o: %.c $(DEPS)
# 	$(CC) -c -o $@ $< $(CFLAGS)

# hellomake: $(OBJ)
# 	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)

codeGenerator.dll: codeGenerator.fs
	$(CC) $(CFLAGS) -a codeGenerator.fs

slpc: main.fs codeGenerator.dll
	$(CC) $(CFLAGS) -r codeGenerator.dll main.fs -o slpc.exe

.PHONY: all
all: codeGenerator.dll slpc

.PHONY: clean

clean:
	rm -f *.{exe,dll}
