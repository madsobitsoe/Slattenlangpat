CC=fsharpc
CFLAGS=--nologo

slpc.exe:
	fsharpc src/ast.fs src/parser.fs src/codeGenerator.fs src/main.fs -o slpc.exe
.PHONY: slpc.exe examples
all: ast.dll parser.dll codeGenerator.dll slpc.exe



.PHONY: examples
examples: slpc.exe examples/add1.slp examples/add_and_sub.slp
	mono slpc.exe examples/add1.slp -o examples/add1
	mono slpc.exe examples/add_and_sub.slp -o examples/add_and_sub


.PHONY: clean
clean:
	rm -f *.{exe,dll}
