open AST
open Parser


// A TestCase is a tuple of input * expected
type TestCase<'a,'b> = ATestCase of 'a * 'b
// A TestResult is a tuple of the same type, but wrapped in Success/Failure
type TestResult<'a,'b> = Success of TestCase<'a, 'b> | Failure of TestCase<'a, 'b> * 'b | InternalError of TestCase<'a, 'b> * string
// A TestSuite is a list of functions and their associated list of TestCases
type TestSuite<'f, 't> = ATestSuite of 'f * List<'t>




// Lift a tuple of input * expected to TestCase<'a,'b>
let returnT = function
    | (input,expected)  -> ATestCase(input,expected)

// Run all the tests
let runTests = function
    | ATestSuite (f, tests) -> List.map f tests

// Takes a function to be tested and creates a test-version of it
let test (f:'a -> Result<'b,string>) = function
    | ATestCase (input,expected) ->
        // Handle positive/negative tests
        match expected with
            | Ok res ->
                let actual = f input
                match actual with
                    | Error msg -> InternalError (ATestCase(input,expected),msg)
                    | Ok _ ->
                        if actual = expected then
                            Success (ATestCase (input,expected))
                        else Failure (ATestCase (input,expected),actual)
            | Error _ ->
                match f input with
                    | Error _ -> Success (ATestCase (input,expected))
                    | actual -> Failure (ATestCase (input,expected), actual)


let testcases'const : TestCase<string,Result<Expr,string>> list =
    [
        "1",  Ok (Const 1;)
        "2",  Ok (Const 2;)
        "3",  Ok (Const 3;)
        "0",  Ok (Const 0;)
        "-1", Ok (Const (-1));
        "01", Ok (Const 1;)
        "10", Ok (Const 10;)
        "123456789",Ok (Const 123456789)
        ] |> List.map returnT

let testcases'add : TestCase<string,Result<Expr,string>> list =
    [
        "0+0", Ok (Add (Const 0, Const 0));
        "1+1", Ok (Add (Const 1, Const 1));
        "2+2", Ok (Add (Const 2, Const 2));
        "3+3", Ok (Add (Const 3, Const 3));
        "4+4", Ok (Add (Const 4, Const 4));
        "1+2+3",Ok (Add (Add (Const 1, Const 2), Const 3)); // left associativity
        "1+2+3+4",Ok (Add (Add (Add (Const 1, Const 2), Const 3), Const 4)); // left associativity
        ] |> List.map returnT

let testcases'add'parens : TestCase<string,Result<Expr,string>> list =
    [

        "(1+2)+3", Ok (Add (Add (Const 1, Const 2), Const 3));
        "(1 + 2) + 3", Ok (Add (Add (Const 1, Const 2), Const 3));
        "1+(2+3)", Ok (Add (Const 1, (Add (Const 2, Const 3))));
        "1+(2+3+4)", Ok (Add (Const 1, (Add (Add (Const 2, Const 3), Const 4))));
        ] |> List.map returnT

let testcases'invalid'parses : TestCase<string,Result<Expr,string>> list =
    [

        "(1", Error "";
        "1)", Error "";
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




// Print failures and errors
let pf = function
    | Failure (ATestCase (i,e),a) ->  printfn "Input: %A\tExpected: %A\tGot: %A" i e a
    | InternalError (ATestCase (i,e),errmsg) ->  printfn "Input: %A\tExpected: %A\tGot: %A" i e errmsg


[<EntryPoint>]
let main args =
    printfn "Tests Passed: %d\nTests Failed: %d\nInternal Errors: %d" passedNum failedNum internalErrorNum
    if failedNum <> 0 then
        printfn "Failed tests:"
        List.iter pf failed
    if internalErrorNum <> 0 then
        printfn "Internal Errors:"
        List.iter pf internalErrors
    // if failedNum <> 0 || internalErrorNum <> 0 then 1 else 0

    0
