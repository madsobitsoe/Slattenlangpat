module ParserTest
open AST
open Parser
open TestUtil

let testcases'const : TestCase<string,Result<Expr,string>> list =
    [
        "1",  Ok (Const (Int 1));
        "2",  Ok (Const (Int 2));
        "3",  Ok (Const (Int 3));
        "0",  Ok (Const (Int 0));
        "-1", Ok (Const (Int (-1)));
        "01", Ok (Const (Int 1));
        "10", Ok (Const (Int 10));
        "123456789",Ok (Const (Int 123456789));
        ] |> List.map returnT

let testcases'add : TestCase<string,Result<Expr,string>> list =
    [
        "0+0", Ok (Add (Const (Int 0), Const (Int 0)));
        "1+1", Ok (Add (Const (Int 1), Const (Int 1)));
        "2+2", Ok (Add (Const (Int 2), Const (Int 2)));
        "3+3", Ok (Add (Const (Int 3), Const (Int 3)));
        "4+4", Ok (Add (Const (Int 4), Const (Int 4)));
        "1+2+3",Ok (Add (Add (Const (Int 1), Const (Int 2)), Const (Int 3))); // left associativity
        "1+2+3+4",Ok (Add (Add (Add (Const (Int 1), Const (Int 2)), Const (Int 3)), Const (Int 4))); // left associativity
        ] |> List.map returnT

let testcases'add'parens : TestCase<string,Result<Expr,string>> list =
    [

        "(1+2)+3", Ok (Add (Add (Const (Int 1), Const (Int 2)), Const (Int 3)));
        "(1 + 2) + 3", Ok (Add (Add (Const (Int 1), Const (Int 2)), Const (Int 3)));
        "(1 + 2) + 3 ", Ok (Add (Add (Const (Int 1), Const (Int 2)), Const (Int 3)));
        "((1 + 2) + 3)", Ok (Add (Add (Const (Int 1), Const (Int 2)), Const (Int 3)));
        "1+(2+3)", Ok (Add (Const (Int 1), (Add (Const (Int 2), Const (Int 3)))));
        "1+(2+3+4)", Ok (Add (Const (Int 1), (Add (Add (Const (Int 2), Const (Int 3)), Const (Int 4)))));
        ] |> List.map returnT

let testcases'invalid'parses : TestCase<string,Result<Expr,string>> list =
    [

        "(1", Error "";
        "1)", Error "";
        "+1", Error "";
        "1+", Error "";
        "1 1", Error "";
        "((((", Error "";
        "let 1 = 1 in 1", Error "";
        ] |> List.map returnT


let parserTests = ATestSuite <| (test parse,
                                 testcases'const
                                 @ testcases'add
                                 @ testcases'add'parens
                                 @ testcases'invalid'parses)

let testResults = runTests parserTests
let passed : TestResult<string,Result<Expr,string>> list =
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
