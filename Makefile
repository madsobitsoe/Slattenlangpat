CC=fsharpc
CFLAGS=--nologo



ast.dll: ast.fs
	$(CC) $(CFLAGS) -a ast.fs
codeGenerator.dll: codeGenerator.fs ast.dll
	$(CC) $(CFLAGS) -r ast.dll -a codeGenerator.fs

slpc: main.fs codeGenerator.dll
	$(CC) $(CFLAGS) -r codeGenerator.dll main.fs -o slpc.exe

.PHONY: all
all: codeGenerator.dll slpc

.PHONY: clean

clean:
	rm -f *.{exe,dll}
