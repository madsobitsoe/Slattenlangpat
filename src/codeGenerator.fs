module CodeGenerator

open AST
// BitConverter.GetBytes
open System.IO


type Register =
    | RAX
    | RCX
    | RDX
    | RBX
    | RSI
    | RDI
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


type Instruction = MOV | ADD | SUB | PUSH | POP
type Operand = REG of Register | IMM of int

// Create the MOD bits. Right now, all is Reg -> Reg
let MOD = 0x3 <<< 6
// Convert a register to a 3-bit value to be used in the RM/Mod byte
let regToBits : (Register -> int) = function
    | RAX -> 0
    | RCX -> 1
    | RDX -> 2
    | RBX -> 3
    | RSP -> 4
    | RBP -> 5
    | RSI -> 6
    | RDI -> 7
    | reg -> failwith <| sprintf "Register %A doesn't fit in 3 bits - not implemented yet" reg

let regToRM = regToBits >> (fun x -> x <<< 3)


let genMovOpCode (op1:Operand) (op2:Operand) =
    // offset for movREG instruction (??? - is it also for mov reg imm?)
    let offset = 0xb8
    match op1,op2 with
    | REG reg1, REG reg2 -> sprintf "INTERNAL ERROR: MOV %A, %A is not implemented yet." reg1 reg2 |> failwith
    | REG reg1, IMM im2 ->
        let movOpCode = offset + regToBits reg1 |> byte
        let immVal = System.BitConverter.GetBytes im2
        Array.append [|movOpCode|] immVal
    | IMM im1, REG reg1 -> sprintf "ERROR: MOV %A, %A is not a valid instruction." im1 reg1 |> failwith
    | IMM im1, IMM im2 -> sprintf "ERROR: MOV %A, %A is not a valid instruction." im1 im2 |> failwith


let allRegisters:Set<Register> = set [RAX; RCX; RDX; RBX; RSI; RDI; RSP; RBP; R8; R9; R10; R11; R12; R13; R14; R15; ]
let allArithRegisters:Set<Register> = allRegisters - (set [RBP;RSP])

let getEmptyRegister (usedRegisters:Register list) =
    let emptyReg =
        allArithRegisters - (set usedRegisters)
        |> Set.toList
        |> List.head
    emptyReg,emptyReg::usedRegisters


// Still in use - TODO REMOVE
let registerToByteMov register =
    let offset = 0xb8
    match register with
        | RAX -> 0 + offset |> byte
        | RCX -> 1 + offset |> byte
        | RDX -> 2 + offset |> byte
        | RBX -> 3 + offset |> byte
        | RSI -> 6 + offset |> byte
        | RDI -> 7 + offset |> byte
        // | RSP -> 5 + offset |> byte
        // | RBP -> 6 + offset |> byte
        | _ -> failwith <| sprintf "MOV FOR %A NOT YET IMPLEMENTED." register




// --------------------------------
// CODE GENERATION
// --------------------------------

// hacky way to print rax
// Move n into rax
let MOVRAX (n:int) = Array.append [|registerToByteMov RAX|] <| System.BitConverter.GetBytes n
let MOVRDX (n:int) = Array.append [|registerToByteMov RDX|] <| System.BitConverter.GetBytes n
let MOVRDI (n:int) = Array.append [|registerToByteMov RDI|] <| System.BitConverter.GetBytes n
let MOVRSIRSP = [|0x48uy;0x89uy;0xe6uy|]
let PUSHREG reg = [|regToBits reg + 0x50 |> byte|]
let PUSHRAX = [|0x50uy|]
// Bytes for a syscall instruction
let SYSCALL = [|0x0fuy; 0x05uy;|]

// Write n bytes located at $rsp
let SYS_WRITE n reg =
    let rdi = genMovOpCode (REG RDI) (IMM 1) // STDOUT
    let rax = genMovOpCode (REG RAX) (IMM 1) // SYS_WRITE
    let rdx = genMovOpCode (REG RDX) (IMM n)
    Array.append rax SYSCALL
    |> Array.append rdx
    |> Array.append MOVRSIRSP
    |> Array.append rdi
    |> Array.append (PUSHREG reg)


// General expressions
let rec genCodeForExpr (usedRegisters:Register list) (e:Expr) : (byte [] * Register list) =
    match e with
        | Const i ->
            printfn "Generating mov for %A\tRegs in use: %A" e usedRegisters
            let reg,usedRegisters' = getEmptyRegister usedRegisters
            // mov imm to r*x
            printfn "mov %A into %A\tRegs in use: %A" i reg usedRegisters'
            let bytesForMov = genMovOpCode (REG reg) (IMM i)
            bytesForMov,usedRegisters'

        | Add (e1,e2) ->
                    printfn "Generating ADD for %A\tRegs in use: %A" e usedRegisters
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
                    // ex1Reg *should* contain result of computing e1.
                    // Assume I can do what I want with all other regs used exclusively in computation of e1
                    let ex1Reg = List.head usedRegisters'
                    printfn "ex1: %A" e1
                    printfn "Reg for ex1: %A" ex1Reg
                    printfn "Regs used by ex1: %A" usedRegisters'
                    let ex2Bytes,usedRegisters'' = genCodeForExpr usedRegisters' e2
                    // Wow, nooot pretty. TODO REPLACE
                    let ex2Reg = List.head usedRegisters''
                    printfn "ex2: %A" e2
                    printfn "Reg for ex2: %A" ex2Reg
                    printfn "Regs used at ex2: %A" usedRegisters''
                    let firstBytes = Array.append ex1Bytes ex2Bytes
                    let add = 0x1uy
                    let addMOD = MOD
                    let addREG = regToBits ex1Reg
                    let addRM = regToRM ex2Reg
                    let addFollowByte = addMOD + addRM + addREG |> byte
                    printfn "Follow byte generated: %x" addFollowByte
                    (Array.append firstBytes [|add;addFollowByte|]), usedRegisters'
        | Sub (e1,e2) ->
            // match (e1,e2) with
                // | Const i1, Const i2 ->
                //     printfn "Generating SUB for %A\tRegs in use: %A" e usedRegisters
                //     // SUB r/m16/32/64	r16/32/64 has opcode 2b
                //     // To generate a SUB, I need to know which registers to add
                //     // So I can generate the following byte (that indicates registers)
                //     let ex1Bytes,usedRegisters' = genCodeForExpr usedRegisters e1
                //     // Wow, nooot pretty. TODO REPLACE
                //     let ex1Reg = List.head usedRegisters'
                //     printfn "Reg for ex1: %A" ex1Reg
                //     let ex2Bytes,usedRegisters'' = genCodeForExpr usedRegisters' e2
                //     // Wow, nooot pretty. TODO REPLACE
                //     let ex2Reg = List.head usedRegisters''
                //     printfn "Reg for ex2: %A" ex2Reg
                //     let firstBytes = Array.append ex1Bytes ex2Bytes
                //     let sub = 0x29uy
                //     let subMOD = MOD
                //     let subREG = regToBits ex1Reg
                //     let subRM = regToRM ex2Reg
                //     let subFollowByte = subMOD + subRM + subREG |> byte
                //     printfn "Follow byte generated: %x" subFollowByte
                //     (Array.append firstBytes [|sub;subFollowByte|]), usedRegisters''
                // | _ -> failwith "Uh Oh - SUB only works with const expressions for now."
            printfn "Generating SUB for %A\tRegs in use: %A" e usedRegisters
            let ex1Bytes,usedRegisters' = genCodeForExpr usedRegisters e1
            // Wow, nooot pretty. TODO REPLACE
            // ex1Reg *should* contain result of computing e1.
            // Assume I can do what I want with all other regs used exclusively in computation of e1
            let ex1Reg = List.head usedRegisters'
            printfn "ex1: %A" e1
            printfn "Reg for ex1: %A" ex1Reg
            printfn "Regs used by ex1: %A" usedRegisters'
            let ex2Bytes,usedRegisters'' = genCodeForExpr usedRegisters' e2
            // Wow, nooot pretty. TODO REPLACE
            let ex2Reg = List.head usedRegisters''
            printfn "ex2: %A" e2
            printfn "Reg for ex2: %A" ex2Reg
            printfn "Regs used at ex2: %A" usedRegisters''
            let firstBytes = Array.append ex1Bytes ex2Bytes
            let sub = 0x29uy
            let subMOD = MOD
            let subREG = regToBits ex1Reg
            let subRM = regToRM ex2Reg
            let subFollowByte = subMOD + subRM + subREG |> byte
            printfn "Follow byte generated: %x" subFollowByte
            (Array.append firstBytes [|sub;subFollowByte|]), usedRegisters'
        | Print e ->
            printfn "Generating code for printing exp."
            let eBytes,usedRegisters' =  genCodeForExpr usedRegisters e
            let sys_writeBytes = SYS_WRITE 8 <| List.head usedRegisters'
            Array.append eBytes (Array.append [|0x90uy;0x90uy;0x90uy;0x90uy;0x90uy;|] sys_writeBytes), usedRegisters'

        // For future use, when I update the AST and forget all about it
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
    let prog = Array.append bytes <| genExit 0
    let header = genHeader prog
    let allBytes = Array.append header prog
    try
        File.WriteAllBytes(filename, allBytes) |> ignore
        sprintf "%d bytes generated and written to %s" (allBytes.Length) filename |> Ok
    with _ -> sprintf "Error when writing bytes to %s" filename |> Error

let compileExpr : (Expr -> Result<byte [],string>)  =  (genCodeForExpr []) >> fst >> Ok
let compileAndWrite filename =
    compileExpr
    >> Result.bind (writeExecutableToDisk filename)
