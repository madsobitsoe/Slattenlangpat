module ParserTest
open AST
open Parser
open TestUtil

let testcases'const : TestCase<string,Result<Program,string>> list =
    [
        "1",  Ok       [SExp (Const (Int 1))];
        "2",  Ok       [SExp (Const (Int 2))];
        "3",  Ok       [SExp (Const (Int 3))];
        "0",  Ok       [SExp (Const (Int 0))];
        "-1", Ok       [SExp (Const (Int (-1)))];
        "01", Ok       [SExp (Const (Int 1))];
        "10", Ok       [SExp (Const (Int 10))];
        "123456789",Ok [SExp (Const (Int 123456789))];
        ] |> List.map returnT

let testcases'add : TestCase<string,Result<Program,string>> list =
    [
        "0+0",    Ok [SExp (Oper (Plus, (Const (Int 0)), Const (Int 0)))];
        "1+1",    Ok [SExp (Oper (Plus, (Const (Int 1)), Const (Int 1)))];
        "2+2",    Ok [SExp (Oper (Plus, (Const (Int 2)), Const (Int 2)))];
        "3+3",    Ok [SExp (Oper (Plus, (Const (Int 3)), Const (Int 3)))];
        "4+4",    Ok [SExp (Oper (Plus, (Const (Int 4)), Const (Int 4)))];
        "1+2+3",  Ok [SExp (Oper (Plus, (Oper (Plus, Const (Int 1), Const (Int 2))), Const (Int 3)))]; // left associativity
        "1+2+3+4",Ok [SExp (Oper (Plus, Oper (Plus, Oper (Plus, Const (Int 1), Const (Int 2)), Const (Int 3)), Const (Int 4)))]; // left associativi)ty
        ] |> List.map returnT

let testcases'add'parens : TestCase<string,Result<Program,string>> list =
    [

        "(1+2)+3"       , Ok [SExp (Oper (Plus, Oper (Plus, Const (Int 1), Const (Int 2)), Const (Int 3)))];
        "(1 + 2) + 3"   , Ok [SExp (Oper (Plus, Oper (Plus, Const (Int 1), Const (Int 2)), Const (Int 3)))];
        "(1 + 2) + 3 "  , Ok [SExp (Oper (Plus, Oper (Plus, Const (Int 1), Const (Int 2)), Const (Int 3)))];
        "((1 + 2) + 3)" , Ok [SExp (Oper (Plus, Oper (Plus, Const (Int 1), Const (Int 2)), Const (Int 3)))];
        "1+(2+3)"       , Ok [SExp (Oper (Plus, Const (Int 1), Oper (Plus, Const (Int 2), Const (Int 3))))];
        "1+(2+3+4)"     , Ok [SExp (Oper (Plus, Const (Int 1), Oper (Plus, Oper (Plus, Const (Int 2), Const (Int 3)), Const (Int 4))))];
        ] |> List.map returnT



let testcases'strings : TestCase<string,Result<Program,string>> list =
    [
        "\"\"", Ok [SExp (Const (String ""))];        
        "\"a\"", Ok [SExp (Const (String "a"))];
        "\"abc\"", Ok [SExp (Const (String "abc"))];
        "\"abc\\ndef\"", Ok [SExp (Const (String "abc\ndef"))];        

    ] |> List.map returnT




let testcases'let'bindings : TestCase<string,Result<Program,string>> list =
    [
        "let a = 2", Ok [SDef ("a", (Const (Int 2)))];
        "let a = 2; a", Ok [SDef ("a", (Const (Int 2))); SExp (Var "a")];
        "let a = 1; let b = 2; let c = 3; a + b + c", Ok [SDef ("a", (Const (Int 1))); SDef ("b", (Const (Int 2)));SDef ("c", (Const (Int 3)));SExp (Oper (Plus, Oper (Plus, Var "a", Var "b"), Var "c"))];        
    ] |> List.map returnT





let testcases'invalid'parses : TestCase<string,Result<Program,string>> list =
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
                                 @ testcases'strings
                                 @ testcases'let'bindings
                                 @ testcases'invalid'parses
                                 )

let testResults = runTests parserTests
let passed : TestResult<string,Result<Program,string>> list =
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
