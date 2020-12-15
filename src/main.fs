open System.IO

open AST
open Parser
open CodeGenerator
open Interpreter


let readFile filename =
    try File.ReadAllText filename |> Ok
    with _ -> sprintf "Error when opening file %s." filename |> Error


let readParseCompileAndWrite inputFilename outputFilename =
    readFile inputFilename
    |> Result.bind parse
    |> Result.bind (compileAndWrite outputFilename)


let rec repl () =
    do printf "SLP> "
    match System.Console.ReadLine() with
    | "exit" -> ()
    | input ->
        parse input
        |> Result.bind eval
        |> Result.bind (fun x -> printfn "%d" x; Ok x )
        |> ignore; repl ()

let usage =
      "Usage: \n\tslpc input.slp [-o executableName]"
    + "\tInteractive mode: slpc -i"


[<EntryPoint>]
let main args =
    let exitcode =
        match args with
        | [|"-i"|] -> printfn "Starting SLP interactive mode..."; repl (); 0
        | [|inputFilePath;"-o";outputFilePath|] ->

            printfn "Will compile %s and save as %s" inputFilePath outputFilePath
            match readParseCompileAndWrite inputFilePath outputFilePath with
                | Ok msg -> printfn "%s" msg; 0
                | Error err -> printfn "%s" err; 1
        | _ -> printfn "%s" usage; 0

    exitcode
