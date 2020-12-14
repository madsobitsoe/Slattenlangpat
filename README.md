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



## Building
You will probably not want to.

You will need:
* An F\# compiler. Both mono and dotnet core will work.
* make (optional)

## Usage
Just don't.


## State of project
Very much in the beginning. Basically nothing is implemented.

Right now the compiler ignores all input.

Addition and subtraction of 32-bit numbers is supported.
Printing is underway. Right now the result of any expression up to 8 bytes can be printed:
The Expr `Print (Const 173034579)` will print `SLP\n` to STDOUT


It can generate an Elf64-binary that sets the statuscode to a supplied value, and exits via sys_exit
