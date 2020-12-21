module Interpreter
open AST
// type EvalError = string
// type EvalResult = int
// type Eval = Result<EvalResult,EvalError>

// An environment is a function
// type Environment = VName -> Result<Expr,string>
// // The empty environment always returns an error
// let initEnv:Environment = (fun name -> Error <| sprintf "Variable %s not found in environment" name)
// // Extending environments means wrapping the previous environment in a new environment
// let extendEnv name expr env = (fun x -> if x = name then Ok expr else env x)
// // Only needed when piping. An environment is a function, so lookup is `flip`
// let lookup name env = env name

// Usage of environments
// initEnv "x"
// |> extendEnv "x" 1
// |> lookup "x" // result is Ok 1

// initEnv
// |> extendEnv "x" 1
// |> lookup "y" // result is Error "Variable y not found in environment"

type Environment = (VName * Value) list

type RunError = ErrBadVar of VName | ErrBadFun of FName
type Comp<'a> = AComp of (Environment -> Result<'a,RunError> * string list)

let runComp comp env : (Result<'a,RunError> * string list) =
    match comp with
    | AComp (comp) -> comp env


module Comp =
    // let map f x =
    //     match x with
    //         | Ok x -> Ok (f x)
    //         | Error err -> Error err
    let ret x = AComp (fun _ -> Ok x,[])
    let bind m f  =
        AComp (fun env ->
               match runComp m env with
               | (Error e, s) -> (Error e, s)
               | (Ok a, s) ->
                   let (a1,s1) = runComp (f a) env in (a1, s @ s1)
               )
    // let bindDiscard m f =
    //     AComp (fun env ->
    //            match runComp m env with
    //            | (Error e, s) -> (Error e, s)
    //            | (Ok a, s) ->
    //                let (a1,_) = runComp (f a) env in (a1, s)
    //            )    // let apply = function
    //     | Error errs1, Error errs2 -> Error (errs1 @ errs2)
    //     | Ok f, Ok x -> Ok (f x)
    //     | _, Error errs -> Error errs
    //     | Error errs,_ -> Error errs
    // let bind  = function
    //     | f,Ok x -> f x
    //     | _,Error errs -> Error errs

let (>>=) = Comp.bind
// let (>>>=) = Comp.bindDiscard
// Doesn't exist in f#, so create the haskell-version ourselves
let lookup name = fun env -> List.tryFind (fun (ename,_) -> ename=name) env |> Option.map snd

let abort err = AComp (fun _ -> Error err,[])

let look name = AComp (fun env ->
                       match lookup name env with
                       | Some x -> Ok x, []
                       | None -> Error (ErrBadVar name),[]
                       )
let withBinding name value comp = AComp (fun env ->
                                         let env' = (name,value)::env in
                                         runComp comp env')

let output s = AComp (fun _ -> Ok (), [s])


let operate op a b =
    match op,a,b with
        | Plus,Int a, Int b -> Ok (a + b |> Int)
        | Minus,Int a, Int b -> Ok (a - b |> Int)
        | _ -> Error "Op not implemented yet"


let apply fname args =
    match fname,args with
    | "print", [] ->
        output "" |> ignore
        Comp.ret Unit
    | "print", (Int i::_) -> sprintf "%d" i |> output >>= (fun _ -> Comp.ret Unit)
    | "print", (String s::_) -> output s >>= (fun _ -> Comp.ret Unit)
    | name,_ -> abort (ErrBadFun name)

let rec eval = function
    | Const c -> Comp.ret c
    | Var name -> look name
    | Oper (op,e1,e2) ->
        eval e1 >>= (fun (res1) ->
        eval e2 >>= (fun (res2) ->
                       match operate op res1 res2 with
                       | Ok v -> Comp.ret v
                       | Error e -> abort (ErrBadVar e)))
    | Call ("print", e::es) ->
        eval e >>= (fun res1 ->
                     apply "print" [res1])
    | _ -> abort (ErrBadVar "NOT IMPLEMENTED")



// let rec eval':(Expr * Environment -> Eval) = function
//     | Const (Int i),env -> Ok i
//     | Add (e1,e2), env ->
//         let res1 = eval' (e1,env)
//         match res1 with
//             | Error err -> Error err
//             | good ->
//                 good >>=
//                     (fun e1res ->
//                      eval' (e2,env) >>= (fun e2res -> e1res + e2res |> Ok))

//     | Sub (e1,e2),env ->
//         eval' (e1,env) >>=
//             (fun e1res ->
//                      eval' (e2,env) >>= (fun e2res -> e1res - e2res |> Ok))

//             // Currently, evaluates to int and prints the first 4 bytes as their ascii-value
//             // This is STUPID AF, but it is what the code generator does.
//             // The interpreter should always mimic the code generator, not the other way around
//             // A print in SLP doesn't "evaluate" to anything
//             // The codegenerator will currently
//             // generate instruction for the expression, store the result in RAX (hopefully), push it to stack and call SYS_WRITE with a pointer to the stack and write 8 bytes. At least four of these will be NULL-bytes.
//             // NOTE: In the interpreter, a Print evaluates to the value of the expression being printed.
//             // F# requires all branches in a function to evaluate to the same type.
//             // Since we don't have any types in SLP (yet), this will have to do.
//             // The Expr below should evaluate `173034579` and print "SLP\n\x00\x00\x00\x00"
//             // eval (Print (Const 173034579))

//     | Print e,env ->
//         eval' (e,env) >>= (fun x ->
//                     // Convert the int32 to 4 bytes and append the 4 null-bytes
//                     Array.append  (System.BitConverter.GetBytes (x)) (System.BitConverter.GetBytes(0))
//                     |> Array.map char
//                     |> Array.map string
//                     |> Array.reduce (+)
//                     // codegenerator will only print a newline if
//                     // (x >>> 24) &&& 0xff = 0xa
//                     // Therefore printf completely by intention
//                     |> (fun s -> printf "%s" s; Ok x))
//         // Expressions are evaluated before they are stored in an environment.
//         // We should definitely have a value type
//     | Var (name),env -> env name >>= (fun res -> eval' (res,env))
//     | Let (name,e1,inExpr),env ->
//         // Evaluate e1 in current env before storing the result - then wrap it in Const. (We NEED a value type)
//         let res1 = eval' (e1,env)
//         match res1 with
//             | Error err -> Error err
//             | good ->
//                 good >>= (fun x ->
//                           let newEnv = extendEnv name (Const (Int x)) env
//                           eval' (inExpr,newEnv)
//              )
//     | unimplemented -> sprintf "The expression '%A' is sadly not implemented yet. \nCheck https://github.com/madsobitsoe/Slattenlangpat/issues and maybe add an issue or submit a pull request." (fst unimplemented) |> Error
// The "public" api.



// let eval = function | x -> eval' (x,initEnv)


let rec exec = function
    | [] -> Comp.ret []
    | s::ss ->
        match s with
            | SDef (name, e) ->
                eval e >>= (fun res ->
                             withBinding name res (exec ss) )
            | SExp e ->
                eval e >>= (fun res -> exec ss)

let execute program =
    let (value,outputs) = runComp (exec program) []
    let errors =
        match value with
            | Error e -> Some e
            | Ok _ -> None
    (outputs, errors)
