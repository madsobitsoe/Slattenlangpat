open CodeGenerator
open AST


let tmpAst1 =
    let e1 = Add (Const 1, Add (Const 2, (Add (Const 3, Add (Const 4, Const 5))))) in
    let e2 = Add (Add (Add (Add (Const 6, Const 7), Const 8), Const 9), Const 10) in
    Add (e1, e2)

let tmpAst2 =
    Sub (Const 2, Sub (Const 5, Sub(Const 4, Sub (Const 3, Const 2))))

let tmpAst3 =
    Sub (Const 10, Add (Const 3, Sub (Const 17, Const 10)))

let tmpAst4 =
    Sub (Sub (Sub (Sub (Sub (Const 15, Const 5), Const 4), Const 3), Const 2), Const 1)

// Should print SLP\n
let tmpAst5 = Print (Const 173034579)

let usage = "Usage: slpc input.slp [-o executableName]"

[<EntryPoint>]
let main args =
    match args with
        | [|inputFilePath;"-o";outputFilePath|] ->

            printfn "Woohoo. Will compile %s and save as %s" inputFilePath outputFilePath
            compileAndWrite tmpAst1 (outputFilePath + "_add")
            compileAndWrite tmpAst2 (outputFilePath + "_sub")
            compileAndWrite tmpAst3 (outputFilePath + "_sub_and_add")
            compileAndWrite tmpAst4 (outputFilePath + "_sub_left_nest")
            compileAndWrite tmpAst5 (outputFilePath + "_print_rax")
        | _ -> printfn "%s" usage

    0
