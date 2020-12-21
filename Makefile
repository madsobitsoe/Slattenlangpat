CC=fsharpc
CFLAGS=--nologo


slpc.exe: src/ast.fs src/parser.fs src/interpreter.fs  src/main.fs
	fsharpc src/ast.fs src/parser.fs src/interpreter.fs src/main.fs -o slpc.exe

# slpc.exe: src/ast.fs src/parser.fs src/interpreter.fs src/codeGenerator.fs src/main.fs
# 	fsharpc src/ast.fs src/parser.fs src/interpreter.fs src/codeGenerator.fs src/main.fs -o slpc.exe


parserTests.exe: src/ast.fs src/parser.fs src/tests/testUtil.fs src/tests/parserTests.fs
	fsharpc src/ast.fs src/parser.fs src/tests/testUtil.fs src/tests/parserTests.fs

interpreterTests.exe: src/ast.fs src/interpreter.fs src/tests/testUtil.fs src/tests/interpreterTests.fs
	fsharpc src/ast.fs src/interpreter.fs src/tests/testUtil.fs src/tests/interpreterTests.fs


.PHONY: test
test: parserTests.exe interpreterTests.exe
	mono parserTests.exe
	mono interpreterTests.exe

.PHONY: testv
testv: parserTests.exe interpreterTests.exe
	mono parserTests.exe -v
	mono interpreterTests.exe -v



.PHONY: examples
examples: slpc.exe examples/add1.slp examples/add_and_sub.slp examples/hello.slp
	mono slpc.exe examples/add1.slp -o examples/add1
	mono slpc.exe examples/add_and_sub.slp -o examples/add_and_sub
	mono slpc.exe examples/hello.slp -o examples/hello

.PHONY: clean
clean:
	rm -f *.{exe,dll}
