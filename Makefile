CC=fsharpc
CFLAGS=--nologo

slpc.exe:
	fsharpc src/ast.fs src/parser.fs src/interpreter.fs src/codeGenerator.fs src/main.fs -o slpc.exe



test: src/tests/parserTests.fs
	fsharpc src/ast.fs src/parser.fs src/tests/parserTests.fs
	mono parserTests.exe



.PHONY: examples
examples: slpc.exe examples/add1.slp examples/add_and_sub.slp
	mono slpc.exe examples/add1.slp -o examples/add1
	mono slpc.exe examples/add_and_sub.slp -o examples/add_and_sub


.PHONY: clean
clean:
	rm -f *.{exe,dll}
