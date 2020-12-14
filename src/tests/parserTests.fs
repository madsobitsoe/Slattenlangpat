open AST
open Parser



let test f testcase =
    let input,expected = testcase
    let actual = f input
    actual = expected



let testcases'const =
    [
        "1",  Ok (Const 1;)
        "2",  Ok (Const 2;)
        "3",  Ok (Const 3;)
        "0",  Ok (Const 0;)
        "-1", Ok (Const (-1));
        "01", Ok (Const 1;)
        "10", Ok (Const 10;)
        "123456789",Ok (Const 123456789)
        ]

let testcases'add =
    [
        "0+0", Ok (Add (Const 0, Const 0));
        "1+1", Ok (Add (Const 1, Const 1));
        "2+2", Ok (Add (Const 2, Const 2));
        "3+3", Ok (Add (Const 3, Const 3));
        "4+4", Ok (Add (Const 4, Const 4));
        "1+2+3",Ok (Add (Add (Const 1, Const 2), Const 3)); // left associativity
        "1+2+3+4",Ok (Add (Add (Add (Const 1, Const 2), Const 3), Const 4)); // left associativity
        ]


[<EntryPoint>]
let main args =

    List.map (test parse) testcases'const
    |> List.reduce (&&)
    |> printfn "All const tests passed: %b"
    List.map (test parse) testcases'add
    |> List.reduce (&&)
    |> printfn "All add tests passed: %b"
    0
