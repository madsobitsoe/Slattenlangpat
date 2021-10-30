open System.IO

open AST
open Parser
open CodeGenerator
open Interpreter


let readFile filename =
    try File.ReadAllText filename |> Ok
    with _ -> sprintf "Error when opening file %s." filename |> Error


// let readParseCompileAndWrite inputFilename outputFilename =
//     readFile inputFilename
//     |> Result.bind parse
//     |> Result.bind (compileAndWrite outputFilename)

let readParseCompileAndWrite inputFilename outputFilename =
    let prog =
        readFile inputFilename
        |> Result.bind parse
    match prog with
        | Ok (SExp e :: es) -> compileAndWrite outputFilename e
        | _ -> failwith "Sorry, you tried to compile stuff not implemented yet"
    // |> Result.bind (compileAndWrite outputFilename)


let rec repl handler =
    do printf "SLPi> "
    match System.Console.ReadLine() with
    | "exit" | "q" | "quit" -> ()
    | input ->
        handler input; repl handler

let pRes = function
    | outputs,Ok value -> printfn "val it : TYPE = %A\n%s" (List.head value) <| List.fold (+) "" outputs
    | outputs,Error msg -> printfn "Output: %s\nErrors:%A" (List.fold (+) "" outputs) msg


let parseResToString = function
    | Ok prog -> sprintf "OK %A" prog
    | Error err -> sprintf "Error: %s" err
    

let rec debug'handler input =
    printfn "Parsing %A" input
    let parseRes = parse input
    printfn "Result of parse: %s" <| parseResToString parseRes
    printfn "Evaluating in the interpreter..."
    match parseRes with
        | Ok prog -> execute prog |> pRes
        | _ -> printfn "Error while parsing. Can not evaluate."

let repl' debug =
    let replmsg =
          "Starting SLP interactive mode..."
        + "\nType exit, quit or q to quit."
    printfn "%s" replmsg
    // let handler res =
    //     match res with
    //         | Ok res -> printfn "%d" res
    //         | Error msg -> printfn "%s" msg
    // if debug then
    repl debug'handler
    // else repl default'handler
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
        | [|"-i"|] ->  repl' true
        // | [|"-i";"-d"|] ->  repl' true
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
