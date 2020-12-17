module TestUtil




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


// Print test results
let pf = function
    | Success (ATestCase (i,e)) -> printfn "[PASSED] Input: %A\nExpected: %A" i e
    | Failure (ATestCase (i,e),a) ->  printfn "[FAILED] Input: %A\nExpected: %A\nGot: %A" i e a
    | InternalError (ATestCase (i,e),errmsg) ->  printfn "[ERROR] Input: %A\nExpected: %A\nGot: %A" i e errmsg
