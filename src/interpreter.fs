module Interpreter

open AST
type EvalError = string
type EvalResult = int
type Eval = Result<EvalResult,EvalError>


// infix for Result.bind, so it looks like I know what I'm doing.
let ( >>= ) r f = r |> Result.bind f


// An environment is a function
type Environment = VName -> Result<Expr,string>
// The empty environment always returns an error
let initEnv:Environment = (fun name -> Error <| sprintf "Variable %s not found in environment" name)
// Extending environments means wrapping the previous environment in a new environment
let extendEnv name expr env = (fun x -> if x = name then Ok expr else env x)
// Only needed when piping. An environment is a function, so lookup is `flip`
let lookup name env = env name

// Usage of environments
// initEnv "x"
// |> extendEnv "x" 1
// |> lookup "x" // result is Ok 1

// initEnv
// |> extendEnv "x" 1
// |> lookup "y" // result is Error "Variable y not found in environment"


// This should evaluate `173034579` and print "SLP\n\x00\x00\x00\x00"
// eval (Print (Const 173034579))



let rec eval':(Expr * Environment -> Eval) = function
    | Const (Int i),env -> Ok i
//    | Value (String s),env -> sprintf "Strings not implemented yet" |> failwith
//    | ((Const i),env) -> Ok i
    | Add (e1,e2), env ->
        let res1 = eval' (e1,env)
        match res1 with
            | Error err -> Error err
            | good ->
                good >>=
                    (fun e1res ->
                     eval' (e2,env) >>= (fun e2res -> e1res + e2res |> Ok))

    | Sub (e1,e2),env ->
        eval' (e1,env) >>=
            (fun e1res ->
                     eval' (e2,env) >>= (fun e2res -> e1res - e2res |> Ok))

            // Currently, evaluates to int and prints the first 4 bytes as their ascii-value
            // This is STUPID AF, but it is what the code generator does.
            // The interpreter should always mimic the code generator, not the other way around
            // A print in SLP doesn't "evaluate" to anything
            // The codegenerator will currently
            // generate instruction for the expression, store the result in RAX (hopefully), push it to stack and call SYS_WRITE with a pointer to the stack and write 8 bytes. At least four of these will be NULL-bytes.
            // NOTE: In the interpreter, a Print evaluates to the value of the expression being printed.
            // F# requires all branches in a function to evaluate to the same type.
            // Since we don't have any types in SLP (yet), this will have to do.
            // The Expr below should evaluate `173034579` and print "SLP\n\x00\x00\x00\x00"
            // eval (Print (Const 173034579))

    | Print e,env ->
        eval' (e,env) >>= (fun x ->
                    // Convert the int32 to 4 bytes and append the 4 null-bytes
                    Array.append  (System.BitConverter.GetBytes (x)) (System.BitConverter.GetBytes(0))
                    |> Array.map char
                    |> Array.map string
                    |> Array.reduce (+)
                    // codegenerator will only print a newline if
                    // (x >>> 24) &&& 0xff = 0xa
                    // Therefore printf completely by intention
                    |> (fun s -> printf "%s" s; Ok x))
        // Expressions are evaluated before they are stored in an environment.
        // We should definitely have a value type
    | Var (name),env -> env name >>= (fun res -> eval' (res,env))
    | Let (name,e1,inExpr),env ->
        // Evaluate e1 in current env before storing the result - then wrap it in Const. (We NEED a value type)
        let res1 = eval' (e1,env)
        match res1 with
            | Error err -> Error err
            | good ->
                good >>= (fun x ->
                          let newEnv = extendEnv name (Const (Int x)) env
                          eval' (inExpr,newEnv)
             )
    | unimplemented -> sprintf "The expression '%A' is sadly not implemented yet. \nCheck https://github.com/madsobitsoe/Slattenlangpat/issues and maybe add an issue or submit a pull request." (fst unimplemented) |> Error
// The "public" api.
let eval = function | x -> eval' (x,initEnv)
