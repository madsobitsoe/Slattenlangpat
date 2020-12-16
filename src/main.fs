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


let rec repl handler =
    do printf "SLPi> "
    match System.Console.ReadLine() with
    | "exit" | "q" | "quit" -> ()
    | input ->
        handler input; repl handler
        // parse input
        // |> Result.bind eval
        // |> handler; repl handler

let pRes = function
    | Ok res -> printfn "%d" res
    | Error msg -> printfn "%s" msg

let rec default'handler = (parse >> Result.bind eval >> pRes)
let rec debug'handler input =
    printfn "Parsing %A" input
    let parseRes = parse input
    printfn "Result of parse: %A" parseRes
    printfn "Evaluating in the interpreter..."
    let eRes = Result.bind eval parseRes
    printf "Result of eval: "
    pRes eRes





let repl' debug =
    let replmsg =
          "Starting SLP interactive mode..."
        + "\nType exit, quit or q to quit."
    printfn "%s" replmsg
    // let handler res =
    //     match res with
    //         | Ok res -> printfn "%d" res
    //         | Error msg -> printfn "%s" msg
    if debug then repl debug'handler
    else repl default'handler
    0


let usage =
      "Usage: \n\tslpc.exe [flags] [input.slp] [-o outputExecutable]\n"
    + "Flags:\n"
    + "\t-h (--help) : Print this message. Same as no flags and input\n"
    + "\t-i : Interactive mode (REPL)\n"
    + "\t-d : Enable debug prints in REPL. Requires -i flag\n"
    + "\t-p : Parse a file and print the result. -p must be used exclusively."


[<EntryPoint>]
let main args =
    let exitcode =
        match args with
        | [|"-i"|] ->  repl' false
        | [|"-i";"-d"|] ->  repl' true
        | [|"-p";inputfile|] ->
            printfn "parsing %s" inputfile
            inputfile |> (readFile >> Result.bind parse >> printfn "%A"); 0
        | [|inputFilePath|] ->
            let outputFilePath = inputFilePath.Split(".").[0]
            printfn "Will compile %s and save as %s" inputFilePath outputFilePath
            match readParseCompileAndWrite inputFilePath outputFilePath with
                | Ok msg -> printfn "%s" msg; 0
                | Error err -> printfn "%s" err; 1
        | [|inputFilePath;"-o";outputFilePath|] ->

            printfn "Will compile %s and save as %s" inputFilePath outputFilePath
            match readParseCompileAndWrite inputFilePath outputFilePath with
                | Ok msg -> printfn "%s" msg; 0
                | Error err -> printfn "%s" err; 1
        | [|"-h"|] | [|"--help"|]
        | _ -> printfn "%s" usage; 0

    exitcode
