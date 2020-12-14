module CodeGenerator

open AST
open System.IO


type Register =
    | RAX
    | RCX
    | RDX
    | RBX
    | RSI
    | RSP
    | RBP
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15

let allRegisters = set [RAX; RCX; RDX; RBX; RSI; RSP; RBP; R8; R9; R10; R11; R12; R13; R14; R15; ]


//let getEmptyRegister (usedRegisters:Set<Register>) =
let getEmptyRegister (usedRegisters:Register list) =
    let emptyReg =
        allRegisters - (set usedRegisters)
        // |> Set.difference allRegisters
        |> Set.toList
        |> List.head
    emptyReg,emptyReg::usedRegisters

let registerToByteMov register =
    match register with
        | RAX -> 0xb8uy
        | RCX -> 0xb9uy
        | _ -> failwith <| sprintf "MOV FOR %A NOT YET IMPLEMENTED." register



let MOD = 0x3 <<< 6
let regToBits = function
    | RAX -> 0
    | RCX -> 1
    | RDX -> 2
    | RBX -> 3
    | RSI -> 4
    | RSP -> 5
    | RBP -> 6
    | reg -> failwith <| sprintf "Register %A doesn't fit in 3 bits - not implemented yet" reg

let regToRM = regToBits >> (fun x -> x <<< 3)

// --------------------------------
// CODE GENERATION
// --------------------------------
//let rec genCodeForExpr (usedRegisters:Set<Register>) (e:Expr) : (byte [] * Set<Register>) =
let rec genCodeForExpr (usedRegisters:Register list) (e:Expr) : (byte [] * Register list) =
    match e with
        | Const i ->
            // mov rax, imm
            let reg,usedRegisters' = getEmptyRegister usedRegisters
            let byteCodeReg = registerToByteMov reg
            [|byteCodeReg; // mov imm to r*x
             byte i;0x00uy;
             0x00uy;0x00uy|],usedRegisters'
        | Add (e1,e2) ->
            match (e1,e2) with
                | Const i1, Const i2 ->
                    // ADD r/m16/32/64	r16/32/64 has opcode 1
                    // To generate an add, I need to know which registers to add
                    // So I can generate the following byte (that indicates registers)
                    // https://wiki.osdev.org/X86-64_Instruction_Encoding#ModR.2FM
                    // [ MOD 2 | REG 3 | RM 3 ]
                    // MOD= 11 for direct reg addressing 64 bit mode
                    // REG =  001 for RCX
                    // RM == 001 for RAX
                    // 0x01 0xc1 -> add rcx, rax
                    // 0x01 (0xc1 - 0x01 + 0x05) -> add ebp, eax
                    let ex1Bytes,usedRegisters' = genCodeForExpr usedRegisters e1
                    // Wow, nooot pretty. TODO REPLACE
                    let ex1Reg = List.head usedRegisters'
                    printfn "Reg for ex1: %A" ex1Reg
                    let ex2Bytes,usedRegisters'' = genCodeForExpr usedRegisters' e2
                    // Wow, nooot pretty. TODO REPLACE
                    let ex2Reg = List.head usedRegisters''
                    printfn "Reg for ex2: %A" ex2Reg
                    let firstBytes = Array.append ex1Bytes ex2Bytes
                    let add = 0x1uy
                    let addMOD = MOD
                    let addREG = regToBits ex1Reg
                    let addRM = regToRM ex2Reg
                    let addFollowByte = addMOD + addRM + addREG |> byte
                    printfn "Follow byte generated: %x" addFollowByte
                    (Array.append firstBytes [|add;addFollowByte|]), usedRegisters''
                | _ -> failwith "Uh Oh - Add only works with const expressions for now."
        | Sub (e1,e2) ->
            match (e1,e2) with
                | Const i1, Const i2 ->
            // SUB r/m16/32/64	r16/32/64 has opcode 2b
            // To generate a SUB, I need to know which registers to add
            // So I can generate the following byte (that indicates registers)
                    let ex1Bytes,usedRegisters' = genCodeForExpr usedRegisters e1
                    // Wow, nooot pretty. TODO REPLACE
                    let ex1Reg = List.head usedRegisters'
                    printfn "Reg for ex1: %A" ex1Reg
                    let ex2Bytes,usedRegisters'' = genCodeForExpr usedRegisters' e2
                    // Wow, nooot pretty. TODO REPLACE
                    let ex2Reg = List.head usedRegisters''
                    printfn "Reg for ex2: %A" ex2Reg
                    let firstBytes = Array.append ex1Bytes ex2Bytes
                    let sub = 0x29uy
                    let subMOD = MOD
                    let subREG = regToBits ex1Reg
                    let subRM = regToRM ex2Reg
                    let subFollowByte = subMOD + subRM + subREG |> byte
                    printfn "Follow byte generated: %x" subFollowByte
                    (Array.append firstBytes [|sub;subFollowByte|]), usedRegisters''
                | _ -> failwith "Uh Oh - SUB only works with const expressions for now."
        | _ -> failwith "Uh Oh"



// --------------------------------
// HEADER STUFF
// --------------------------------


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
    let body,regsLeft = genCodeForExpr [] (Sub (Const 1,Const 2))
    let prog = Array.append body <| genExit 42
    let header = genHeader prog
    let allBytes = Array.append header prog
    File.WriteAllBytes(filename, allBytes)
