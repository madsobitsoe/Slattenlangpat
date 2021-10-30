module Interpreter
open AST

type Environment = (VName * Value) list

type RunError = ErrBadVar of VName | ErrBadFun of FName | ErrUndefinedOp of Op * Value * Value
type Comp<'a> = AComp of (Environment -> Result<'a,RunError> * string list)

let runComp comp env : (Result<'a,RunError> * string list) =
    match comp with
    | AComp (comp) -> comp env


module Comp =
    let ret x = AComp (fun _ -> Ok x,[])
    let bind m f  =
        AComp (fun env ->
               match runComp m env with
               | (Error e, s) -> (Error e, s)
               | (Ok a, s) ->
                   let (a1,s1) = runComp (f a) env in (a1, s @ s1)
               )

let (>>=) = Comp.bind

// Lookup doesn't exist in f#, so create the haskell-version ourselves
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


// Handle binary operations
let operate op a b =
    match op,a,b with
        | EQ, Bool a, Bool b -> Ok (a = b |> Bool)
        | NotEQ, Bool a, Bool b -> Ok (a <> b |> Bool)
        | EQ, Int a, Int b -> Ok (a = b |> Bool)
        | NotEQ, Int a, Int b -> Ok (a <> b |> Bool)
        | LT, Int a, Int b -> Ok (a < b |> Bool)
        | GT, Int a, Int b -> Ok (a > b |> Bool)
        | LTE, Int a, Int b -> Ok (a <= b |> Bool)
        | GTE, Int a, Int b -> Ok (a >= b |> Bool)
        | EQ, String a, String b -> Ok (a = b |> Bool)
        | NotEQ, String a, String b -> Ok (a <> b |> Bool)
        | Plus,Int a, Int b -> Ok (a + b |> Int)
        | Minus,Int a, Int b -> Ok (a - b |> Int)
        | Plus, String a, String b -> Ok (a + b |> String)
        | op, a, b -> Error (ErrUndefinedOp (op, a, b))


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
                       | Error e -> abort e))
    | Call ("print", e::es) ->
        eval e >>= (fun res1 ->
                     apply "print" [res1])
    | _ -> abort (ErrBadVar "NOT IMPLEMENTED")



let rec exec = function
    | [] -> Comp.ret []
    | (SExp s)::[] -> eval s >>= (fun res -> Comp.ret [res])
    | s::ss ->
        match s with
            | SDef (name, e) ->
                eval e >>= (fun res ->
                             withBinding name res (exec ss) )
            | SExp e ->
                eval e >>= (fun res -> exec ss)


// TODO Change this
// such that on successful evaluation, the (last) result is returned as well
// Changing Option to Result is probably enough
let execute program =
    let (value,outputs) = runComp (exec program) []
    // let errors =
    //     match value with
    //         | Error e -> Error e
    //         | Ok res -> Ok res
    (outputs, value)
