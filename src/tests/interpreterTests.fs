module InterpreterTest
open AST
open Interpreter
open TestUtil

let testcases'const : TestCase<Expr,Result<EvalResult,string>> list =
    [
        Const 0,  Ok 0;
        Const 1,  Ok 1;
        Const 2,  Ok 2;
        Const 42,  Ok 42
        ] |> List.map returnT


let testcases'add : TestCase<Expr,Result<EvalResult,string>> list =
    [
        Add (Const 0, Const 0),  Ok 0;
        Add (Const 1, Const 0),  Ok 1;
        Add (Const 0, Const 1),  Ok 1;
        Add (Const 2, Const 0),  Ok 2;
        Add (Const 0, Const 2),  Ok 2;
        Add (Const 1, Const 1),  Ok 2;
        Add (Const 1, Const 2),  Ok 3;
        Add (Const 2, Const 1),  Ok 3;
        Add (Const 1, Add (Const 1, Const 1)),  Ok 3;
        Add (Add (Const 1, Const 1), Const 1),  Ok 3;
        Add (Const 1, Add (Const 2, Const 3)),  Ok 6;
        Add (Add (Const 1, Add (Const 2, Const 3)), Const 4),  Ok 10;

        ] |> List.map returnT


let allTests = ATestSuite <| (test eval,
                              testcases'const
                              @ testcases'add)
                                 // @ testcases'add'parens
                                 // @ testcases'invalid'parses)

let testResults = runTests allTests
let passed : TestResult<Expr,Result<EvalResult,string>> list =
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
