module InterpreterTest
open AST
open Interpreter
open TestUtil

let testcases'const : TestCase<Program,Result<string list * Result<Value list,RunError>,string>> list =
    [
        [SExp (Const (Int 0))],   Ok ([],Ok [Int 0]);
        [SExp (Const (Int 1))],   Ok ([], Ok [Int 1]);
        [SExp (Const (Int 2))],   Ok ([], Ok [Int 2]);
        [SExp (Const (Int 42))],  Ok ([], Ok [Int 42])
        ] |> List.map returnT

let testcases'add : TestCase<Program,Result<string list * Result<Value list,RunError>,string>> list =
    [
        [SExp (Oper (Plus, Const (Int 0), Const (Int 0)))],  Ok ([], Ok [Int 0]);
        [SExp (Oper (Plus, Const (Int 1), Const (Int 0)))],  Ok ([], Ok [Int 1]);
        [SExp (Oper (Plus, Const (Int 0), Const (Int 1)))],  Ok ([], Ok [Int 1]);
        [SExp (Oper (Plus, Const (Int 2), Const (Int 0)))],  Ok ([], Ok [Int 2]);
        [SExp (Oper (Plus, Const (Int 0), Const (Int 2)))],  Ok ([], Ok [Int 2]);
        [SExp (Oper (Plus, Const (Int 1), Const (Int 1)))],  Ok ([], Ok [Int 2]);
        [SExp (Oper (Plus, Const (Int 1), Const (Int 2)))],  Ok ([], Ok [Int 3]);
        [SExp (Oper (Plus, Const (Int 2), Const (Int 1)))],  Ok ([], Ok [Int 3]);
        [SExp (Oper (Plus, Const (Int 1), Oper (Plus, Const (Int 1), Const (Int 1))))],  Ok ([], Ok [Int 3]);
        [SExp (Oper (Plus, Oper (Plus, Const (Int 1), Const (Int 1)), Const (Int 1)))],  Ok ([], Ok [Int 3]);
        [SExp (Oper (Plus, Const (Int 1), Oper (Plus, Const (Int 2), Const (Int 3))))],  Ok ([], Ok [Int 6]);
        [SExp (Oper (Plus, Oper (Plus, Const (Int 1), Oper (Plus, Const (Int 2), Const (Int 3))), Const (Int 4)))],  Ok ([], Ok [Int 10]);
        ] |> List.map returnT



let testcases'sub : TestCase<Program,Result<string list * Result<Value list,RunError>,string>> list =
    [
        [SExp (Oper (Minus, Const (Int 0), Const (Int 0)))],  Ok ([], Ok [Int 0]);
        [SExp (Oper (Minus, Const (Int 1), Const (Int 0)))],  Ok ([], Ok [Int 1]);
        [SExp (Oper (Minus, Const (Int 1), Const (Int 1)))],  Ok ([], Ok [Int 0]);
        [SExp (Oper (Minus, Const (Int 0), Const (Int 1)))],  Ok ([], Ok [Int (-1)]);
        [SExp (Oper (Minus, Oper (Minus, Const (Int 5), Const (Int 4)), Const (Int 1)))],  Ok ([], Ok [Int 0]);
    ] |> List.map returnT


// wrap results from execute in Ok, so it fits with the testsuite
let allTests = ATestSuite <| (test (execute >> Ok),
                              testcases'const
                              @ testcases'add
                              @ testcases'sub)

let testResults = runTests allTests
let passed : TestResult<Program,Result<string list * Result<Value list,RunError>,string>> list =
    testResults |> List.choose (function | Success tc -> Some (Success tc) | _ -> None)

let failed = testResults |> List.choose (function | Failure (tc,got) -> Some (Failure (tc,got)) | _ -> None)
let internalErrors = testResults |> List.choose (function | InternalError (tc,errmsg) -> Some (InternalError (tc,errmsg)) | _ -> None)

let passedNum = List.length passed
let failedNum = List.length failed
let internalErrorNum = List.length internalErrors



[<EntryPoint>]
let main args =
    printfn "Tests Passed: %d\nTests Failed: %d\nInternal Errors: %d" passedNum failedNum internalErrorNum
    match args with
        [|"-v"|] -> printfn "Passed tests:" ; List.iter pf passed
        | _ -> ()
    if failedNum <> 0 then
        printfn "Failed tests:"
        List.iter pf failed
    if internalErrorNum <> 0 then
        printfn "Internal Errors:"
        List.iter pf internalErrors

    0
