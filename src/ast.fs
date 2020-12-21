module AST


type Value = Int of int | String of string
type VName = string
type FName = string
type Op = Plus | Minus

type Expr =
    | Const of Value
    | Var of VName
    | Oper of Op * Expr * Expr
    | Let of VName * Expr * Expr
    | Call of VName * Expr list
    | Print of Expr

type Statement =
    | SDef of VName * Expr
    | SExp of Expr

type Program = Statement list
