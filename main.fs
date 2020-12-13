open CodeGenerator



let compileAndWrite filename = CodeGenerator.writeExecutableToDisk filename [||]


[<EntryPoint>]
let main args =
    match args with
        | [|inputFilePath|] ->
            printfn "woohoo, will compile!"
            printfn "writing bytes to disk..."

        | [|inputFilePath;"-o";outputFilePath|] ->

            printfn "Woohoo. Will compile %s and save as %s" inputFilePath outputFilePath
            compileAndWrite outputFilePath
        | _ -> printfn "Usage: slpc input.slp [-o executableName]"

    0
