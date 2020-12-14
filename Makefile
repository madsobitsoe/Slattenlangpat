CC=fsharpc
CFLAGS=--nologo


slpc.exe: main.fs parser.dll codeGenerator.dll
	$(CC) $(CFLAGS) -r parser.dll -r codeGenerator.dll main.fs -o slpc.exe

ast.dll: ast.fs
	$(CC) $(CFLAGS) -a ast.fs

parser.dll: parser.fs ast.dll
	$(CC) $(CFLAGS) -r ast.dll -a parser.fs
codeGenerator.dll: codeGenerator.fs ast.dll
	$(CC) $(CFLAGS) -r ast.dll -a codeGenerator.fs





.PHONY: all
all: parser.dll codeGenerator.dll slpc.exe

.PHONY: clean

clean:
	rm -f *.{exe,dll}


.PHONY: examples
examples: slpc.exe examples/add1.slp examples/add_and_sub.slp
	mono slpc.exe examples/add1.slp -o examples/add1
	mono slpc.exe examples/add_and_sub.slp -o examples/add_and_sub
