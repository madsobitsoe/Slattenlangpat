module CodeGenerator

open System.IO

// The x86-64 asm (nasm) code for the header we want to write.
// calling nasm in flat binary mode with this input will generate a minimal ELF64 executable

// BITS 64
//   org 0x400000

// ehdr:           ; Elf64_Ehdr
//   db 0x7f, "ELF", 2, 1, 1, 0 ; e_ident
//   times 8 db 0
//   dw  2         ; e_type
//   dw  0x3e      ; e_machine
//   dd  1         ; e_version
//   dq  _start    ; e_entry
//   dq  phdr - $$ ; e_phoff
//   dq  0         ; e_shoff
//   dd  0         ; e_flags
//   dw  ehdrsize  ; e_ehsize
//   dw  phdrsize  ; e_phentsize
//   dw  1         ; e_phnum
//   dw  0         ; e_shentsize
//   dw  0         ; e_shnum
//   dw  0         ; e_shstrndx
//   ehdrsize  equ  $ - ehdr

// phdr:           ; Elf64_Phdr
//   dd  1         ; p_type
//   dd  5         ; p_flags
//   dq  0         ; p_offset
//   dq  $$        ; p_vaddr
//   dq  $$        ; p_paddr
//   dq  filesize  ; p_filesz
//   dq  filesize  ; p_memsz
//   dq  0x1000    ; p_align
//   phdrsize  equ  $ - phdr

// _start:
// THE CODE OF OUR PROGRAM GOES HERE
// ; Has to end with this line. nasm converts it the actual bytesize (which we will have to do manually
// filesize  equ  $ - $$

let header'part1 : byte [] =
    [|
     // Elf64_Ehdr
     0x7fuy;0x45uy;0x4cuy;0x46uy; // 0x7fELF
     0x02uy;0x01uy;0x01uy;0x00uy; // e_ident
     0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy; // "times 8 db 0"
     0x02uy;0x00uy; // e_type
     0x3euy;0x00uy; //e_machine
     0x01uy;0x00uy;0x00uy;0x00uy; // e_version
     0x78uy;0x00uy;0x40uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;  // e_entry
     0x40uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;  //e_phoff
     0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy; // e_shoff
     0x00uy;0x00uy;0x00uy;0x00uy; // e_flags
     0x40uy;0x00uy; //e_ehsize - FILL IN WITH ACTUAL HEADER SIZE
     0x38uy;0x00uy; // e_phentsize - FILL IN WITH ACTUAL HEADER SIZE
     0x01uy;0x00uy; // e_phnum
     0x00uy;0x00uy; // e_shentsize
     0x00uy;0x00uy; //e_shnum
     0x00uy;0x00uy; //e_shstrndx
     // Elf64_Phdr
     0x01uy;0x00uy;0x00uy;0x00uy; // p_type
     0x05uy;0x00uy;0x00uy;0x00uy; // p_flags
     0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy; // p_offset
     0x00uy;0x00uy;0x40uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy; // p_vaddr - FILL IN WITH ACTUAL SIZE
     0x00uy;0x00uy;0x40uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy; // p_paddr - FILL IN WITH ACTUAL SIZE
     |]

     // 0x78uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;  // pfilesz - FILL IN WITH ACTUAL FILESIZE
     // 0x78uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy; // p_memsz - FILL IN WITH ACTUAL FILESIZE
let header'part3 =
    [|
     // p_align
     0x00uy;0x10uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;
     |]


let genHeader (prog: byte []) : byte [] =
    let headerSize = 120
    let progSize = prog.Length
    let sizeAsBytes =
        let tmp = System.BitConverter.GetBytes(headerSize + progSize) in
        match tmp.Length with
        | 8 -> tmp
        | less when less < 8 -> Array.append tmp (Array.init (8 - less) (fun _ -> 0x00uy))
        | _ -> failwith "Cannot fit progsize into 64 bits"

    let h1 = Array.append header'part1 sizeAsBytes
    let h2 = Array.append h1 sizeAsBytes
    let h3 = Array.append h2 header'part3
    h3


// gen assembly for syscall sys_exit with statusCode
let genExit statusCode : byte [] =
    [|
     0xb8uy;
     0x3cuy;
     0x00uy;
     0x00uy;
     0x00uy;
     0xbfuy;
     byte statusCode;
     0x00uy;
     0x00uy;
     0x00uy;
     0x0fuy;
     0x05uy;
     |]

let writeExecutableToDisk (filename:string) (bytes:byte []) =
    let prog = genExit 42
    let header = genHeader prog
    let allBytes = Array.append header prog
    File.WriteAllBytes(filename, allBytes)
