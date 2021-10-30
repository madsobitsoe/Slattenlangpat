module AST


type Value = Bool of bool | Int of int | String of string | Unit
type VName = string
type FName = string
type Op = Plus | Minus | EQ | LT | GT | LTE | GTE | NotEQ

type Expr =
    | Const of Value
    | Var of VName
    | Oper of Op * Expr * Expr
    | Call of VName * Expr list
    | Print of Expr
    | Match of Expr * (Expr * Expr) list

type Statement =
    | SDef of VName * Expr
    | SExp of Expr

type Program = Statement list
