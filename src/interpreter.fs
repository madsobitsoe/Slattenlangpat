module Interpreter

open AST
type EvalError = string
type EvalResult = int
type Eval = Result<EvalResult,EvalError>

let ( >>= ) r f = r |> Result.bind f

let a : Result<int,string> = Ok 1
a >>= (fun x -> Ok x)




let rec eval:(Expr->Eval) = function
    | Const i -> Ok i
    | Add (e1,e2) ->
        eval e1 >>=
            (fun e1res ->
                     eval e2 >>= (fun e2res -> e1res + e2res |> Ok))

    | Sub (e1,e2) ->
        eval e1 >>=
            (fun e1res ->
                     eval e2 >>= (fun e2res -> e1res - e2res |> Ok))

    // Currently, evaluates to int and prints the first 4 bytes as their ascii-value
    // This is STUPID AF, but it is what the code generator does.
    // The interpreter should always mimic the code generator, not the other way around
            // A print doesn't "evaluate" to anything
            // The codegenerator will currently
            // generate instruction for the expression, store the result in RAX (hopefully), push it to stack and call SYS_WRITE with a pointer to the stack.
            // NOTE: In the interpreter, a Print evaluates to the value of the expression being printed.
    | Print e ->
        eval e >>= (fun x ->
                    // Convert the int32 to 4 bytes
                    System.BitConverter.GetBytes (x)
                    |> Array.map char
                    |> Array.map string
                    |> Array.reduce (+)
                    // codegenerator will only print a newline if
                    // (x >>> 24) &&& 0xff = 0xa
                    // Therefore printf completely by intention
                    |> (fun s -> printf "%s" s; Ok x))
