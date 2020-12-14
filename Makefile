CC=fsharpc
CFLAGS=--nologo


slpc: main.fs codeGenerator.dll
	$(CC) $(CFLAGS) -r codeGenerator.dll main.fs -o slpc.exe

ast.dll: ast.fs
	$(CC) $(CFLAGS) -a ast.fs

parser.dll: parser.fs
	$(CC) $(CFLAGS) -r ast.dll -a parser.fs
codeGenerator.dll: codeGenerator.fs ast.dll
	$(CC) $(CFLAGS) -r ast.dll -a codeGenerator.fs


.PHONY: all
all: parser.dll codeGenerator.dll slpc

.PHONY: clean

clean:
	rm -f *.{exe,dll}
