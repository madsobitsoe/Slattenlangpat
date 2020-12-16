# Slattenlangpat
A terrible compiler project with no use at all.

## What
A language and compiler.
I'm doing this to:
* Learn about compiler (design)
* Learn about x86-64 assembly
* Learn about Linux and how user-programs interact with the OS
* Learn about LibC (by reimplementing needed parts)

### Language
Slattenlangpat is a language with an F\#/Ocaml-like syntax. (Which I have not designed or made yet)

#### Name
[Slattenlangpat](https://da.wikipedia.org/wiki/Slattenpatten) is an ugly old elf in danish folklore.
The compiler makes ugly Elf64 binaries.

### Compiler
The compiler is written in F\#.
It generates Elf64 binaries.
One of the goals is to create the tiniest possible executables - optimised for size, not speed. (Right now just making broken, but valid, headers)
It generates dependency-free binaries. No LibC here.

### Parser
I will probably need to define the syntax soon. 
Right now, the parser will parse an expression. 
```
VName = string literal
Expr := 
     | Const (int32)
     | Add Expr Expr
     | Sub Expr Expr
     | Print Expr
     | Let VName Expr Expr
     | '(' Expr ')'
```

See the examples/ folder for .slp-program sources. 

### Interpreter/REPL
Start the REPL with `mono slpc.exe -i`. 
Enter an expression, press RET and see the beautiful result!.
`Print EXPR` will convert the result of evaluating EXPR to 8 bytes and print the lower 4 of them. `Print 97` will print 'a' and `Print 10` will print a newline.
As nested let-bindings are allowed, the following works. (HAH!)
```
SLPi> let a = print 97 in let b = print 98 in let c = print 99 in print 10
abc
10
```
The above prints 10 last, because the REPL prints the result of evaluating the expression. `print 10` or `let x = 10 in print x` is thus "equivalent" to `let x = 10 in printf "%c" (char x); x` in F\#.



## Building
You will probably not want to.

You will need:
* An F\# compiler. I use mono. `dotnet core` will probably need an .fsproj file, but should be doable.
* make 

## Usage
Just don't.


## State of compiler
Very much in the beginning. Basically nothing is implemented.

As of right now, the compiler will: 
1. Read a filename and a destination from STDIN, 
2. Parse the file and generate an AST.  
3. translate and generate the program incl. header as bytes
4. Write the binary to disk at the designated path

Generated programs can be run by setting the execute permission flag, and plain executing it. 
I recommend running SLP-programs through gdb (with something like peda installed). Since the binaries don't have output (yet), they are much more interesting when you can step through them.


Addition and subtraction of 32-bit numbers is supported.
Printing is underway. Right now the result of any expression up to 4 bytes can be printed:
The Expr `Print (Const 173034579)` will print `SLP\n` to STDOUT
(It will in fact push 8 bytes to the stack, but since I still use signed 32-bit ints in F#, I can not supply a number larger than 4 bytes.)

It can generate an Elf64-binary that sets the statuscode to a supplied value, and exits via sys_exit
