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

The parser is largely based on this [series](https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/) and my very limited experience with Parsec in Haskell.
It is incredibly slow.

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

#### Hello, World!
This means we can finally write a "Hello world!"-program.
```
(print 65 + 7) + (print 101) + (print 108) + (print 108) + (print 111) + (print 10)
```
Or an even cooler version, that currently takes minutes to evaluate in the interpreter (on my machine). I suspect the parser is a slow AF bottleneck.
```
let H = (65 + 7) in let e = 101 in let l = 108 in let o = 111 in let nl = 10 in \
(print H) + (print e) + (print l) + (print l) + (print o) + (print nl)
```
(The let-binding version doesn't work in the compiler - yet).
## Building
You will probably not want to.

You will need:
* An F\# compiler. I use mono. `dotnet core` will probably need an .fsproj file, but should be doable.
* make

## Usage
Just don't.

If you dare, the options are:

``` shell
mono slpc.exe [flags] [input.slp] [-o outputExecutable]

```
**Flags**
* `-h` or `--help` : Print usage message. (The same as no flags or input).
* `-i` : Start the interactive mode (REPL)
* `-d` : Enable debug prints in REPL. Requires -i flag.
* `-p` : Parse a file and print the result. `-p` must be used exclusively.





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

## Syntax

``` shell
Program ::= Stmts
Stmts ::= Stmt ';'
        | Stmt ';' Stmts
Stmt  ::= 'let' ident '=' Expr
        | Expr

Expr  ::= '()'
        | intConst
        | '"' stringConst '"' 
        | ident
        | 'true' | 'false'
        | Expr Oper Expr
        | ident Exprs
        | '(' Expr ')'
        | 'match' Expr 'with' 
Oper  ::= '+' 
        | '-'
        | '='
        | '<'
        | '>'
        | '<='
        | '>='
        | '<>'

Exprs ::=
	    | Expr
        | Expr Exprs

MCase  '|' Expr '->' Expr 
MCases ::= MCase
        | MCase MCases
```

### Elaborations
#### ident
#### Binary Operations
`a > b` is syntactic sugar for `b < a`.  
`a >= b` is syntactic sugar for `b <= a`.  

#### Match expressions
The language will have match-expressions instead of `if-else`.
Currently the semantics are wild (and not at all final):  
In a match-expression, the following happens:  
- The expression between `match` and `with` is evaluated and typed. 
- All the left-hand-side expressions of the match-cases are evaluated and typed, then checked to be of the same type as the expression to match with.
- All the right-hand-side expressions are typed, then checked to be of the same type.

There is no structural matching, only equality.
There is no bindings on LHS of a match-case. LHS is an expression that will be evaluated and checked for equality. 

``` shell
let a = 42; 
match 41 + 1 with
| 41 -> print "no match\n"
| 41+2 -> print "no match\n"
| a -> print "this will match\n";
```

There are no checks of exhaustive matching, i.e. it is very possible to write a typed match-expression that will fail at runtime. 
``` shell
match true with | false -> print "oh boy\n";
```


As match-expressions evaluate to some value, they can be used for assignments.
The following will print `"The answer!"` with a newline:
``` shell
let a = match true with | false -> 0 | true -> 42; match a with | 0 -> print "oh no\n" | 42 -> print "The answer!\n";
```


