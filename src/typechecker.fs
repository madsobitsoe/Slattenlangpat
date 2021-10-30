module TypeChecker

open AST

type TypedEnv = (VName * TypedExpr) list


// ======================================================================
// Helper functions
// ======================================================================
let getType : (TypedExpr -> BasicType) = function
    | TypedConst (_,t) -> t
    | TypedVar (_,t) -> t
    | TypedOper (_,t) -> t
    | TypedPrint (_,t) -> t
    | TypedCall (_,t) -> t
    | TypedMatch (_,t) -> t
        

let rec flattenResultTypedExprs acc typedExprs : Result<TypedExpr list, string> =
    match typedExprs with
        | [] -> Ok acc
        | x::xs ->
            match x with
                | Ok x -> flattenResultTypedExprs (x::acc) xs 
                | Error e -> Error e


// ======================================================================
// Building typed expressions, statements and programs
// ======================================================================                
let rec typeExpr : ((Expr * TypedEnv) -> Result<TypedExpr, string>) = function
    // Constants are pretty easy. We can use the type constructor straight from the parser/AST
    | Const value,_ ->
        match value with
            | Bool _ ->  Ok (TypedConst (value, BoolT))
            | Int _ ->  Ok (TypedConst (value, IntT))
            | String _ -> Ok (TypedConst (value, StringT))
            | Unit -> Ok (TypedConst (Unit, UnitT))
    // For vars, we need the environment, so we can look up the type
    // (which should already be decided at this point)
    | Var name, env ->
        match List.tryFind (fun (vname, e) -> name = vname) env with
            | Some (_,t) -> Ok (t)
            | None -> Error <| sprintf "Variable %s doesn't exist in env" name
    // For binary operators, get the type of both expressions, check they're the same, assign it to operator
    | Oper (op, e1, e2), env ->
        typeExpr (e1, env)
        |> Result.bind (fun (te1) ->
                                typeExpr (e2, env)
                                |> Result.bind (fun (te2) ->
                                                let te1t = getType te1
                                                let te2t = getType te2
                                                if te1t = te2t then Ok (TypedOper ((op, te1, te2), te1t))
                                                else Error <| sprintf "Types %A and %A do no match" te1t te2t
                                        ))
    // Print is not used anymore, but I didn't realize until I wrote the type-building
    | Print e, env ->
        typeExpr (e, env)
        |> Result.bind (fun te -> Ok (TypedPrint (te, UnitT)))
    // Call is complicated
    | Call (name, exprs), env ->
        // First, find type all expressions/args
        let typedExprs = List.map (fun x -> typeExpr (x,env)) exprs
        // If any of that failed, return error 
        match List.tryFind (fun x -> match x with | Error e -> true | Ok _ -> false) typedExprs with
            | Some e -> e
            // Else, flatten the result-list, check they're all the same,
            // And finally return a TypedCall expression
            | None ->
                flattenResultTypedExprs [] typedExprs
                |> Result.bind (fun typedExprs ->
                                let types = List.map getType typedExprs
                                let first = List.head types
                                let rest = List.tail types
                                if List.forall (fun x -> x = first) rest then
                                   Ok (TypedCall ((name, typedExprs), first))
                                else Error <| sprintf "Types %A do not match" (List.distinct types))
    // Match is a bit complicated
    | Match (me, mcs), env ->
        // First we need to typecheck the match-expression (between match and with)
        typeExpr (me, env)
        |> Result.bind (fun tme ->
                        let cases,results = List.unzip mcs
                        let typedCases = List.map (fun x -> typeExpr (x,env)) cases
                        flattenResultTypedExprs [] typedCases
                        |> Result.bind (fun typedCases ->
                                        let types = List.map getType typedCases
                                        let first = getType tme
                                        if List.forall (fun x -> x = first) types then
                                        // Check the result-cases
                                            let typedResults = List.map (fun x -> typeExpr (x,env)) results
                                            flattenResultTypedExprs [] typedResults
                                            |> Result.bind (fun typedResults ->
                                                            let types = List.map getType typedResults
                                                            let first = List.head types
                                                            let rest = List.tail types
                                                            if List.forall (fun x -> x = first) rest then
                                                                // Empty list for now, as I'm tiiired
                                                                Ok (TypedMatch ((tme, []), first))
                                                            else Error <| sprintf "Types %A do not match" (List.distinct types))

                                        else Error <| sprintf "Types %A do not match" (List.distinct types)))
                        
        // Then we need to typecheck all the match-cases
        // Then check that all the results are the same type (which will be the type of the entire Match construct)
        
    
    // | _ -> Error "untypeable"


let rec typeStatement : ((Statement * TypedEnv) -> Result<(TypedStatement * TypedEnv), string>) = function
    | SExp expr, env -> typeExpr (expr, env) |> Result.bind (fun x -> Ok (TypedSExp x, env))
    | SDef (name, expr), env ->
        typeExpr (expr, env) |> Result.bind (fun (te) -> Ok (TypedSDef (name, te), (name,te)::env))


let rec typeProgram' : ((Program * TypedEnv * TypedProgram) -> Result<TypedProgram, string>) = function
    | [],env,acc -> Ok acc
    | s::ss,env,acc ->
        typeStatement (s,env)
        |> Result.bind (fun (ts,te) -> typeProgram' (ss,te,ts::acc))        


    
let typeProgram : (Program -> Result<TypedProgram, string>) = function
    | [] -> Error "Program was empty, cannot type it!"
    | x -> typeProgram' (x, [], []) |> Result.map List.rev
    

// typeProgram [SDef ("x", Const (Int 1)); SExp (Var "x"); SDef ("y", Const (Int 2)); SExp (Oper (Plus, Var "x", Var "y"))]

// typeProgram [SDef ("x", Const (Int 1)); SExp (Var "x"); SDef ("y", Const (Int 2)); SExp (Oper (Plus, Var "x", Var "y")); SDef ("x", Const (String "yooo")); SExp (Oper (Plus, Var "x", Var "x"))]


// typeProgram [SExp (Match (Const (Int 1), [(Const (Int 1), Const (String "a"))]))]
// typeProgram [SExp (Match (Const (Int 1), [(Const (Int 1), Const (String "a"));(Const (Int 1), Const (Int 2))]))]


// // Stupid testing        
// typeStatement (SDef ("x", Const (Int 1)), [])
// |> Result.bind (fun (_,e) -> typeStatement ((SDef ("y", Const (Int 2)),e)))
// |> Result.bind (fun (_,e) -> typeStatement ((SDef ("z", Const (String "hey")),e)))
// |> Result.bind (fun (_,e) -> typeStatement ((SExp (Var "x")),e))
// |> Result.bind (fun (_,e) -> typeStatement ((SExp (Var "y")),e))
// |> Result.bind (fun (_,e) -> typeStatement (SExp (Oper (Plus, Const (Int 1), Const (Int 2))),e))
// |> Result.bind (fun (_,e) -> typeStatement (SExp (Oper (Plus, Const (Int 1), Const (String "hey"))),e))
// // |> Result.bind (fun (_,e) -> typeStatement ((SExp (Var "ged")),e))

