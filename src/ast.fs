module AST


type VName = string
type Value = Int of int | String of string

type Expr =
    | Const of Value
    | Add of (Expr * Expr)
    | Sub of (Expr * Expr)
    | Var of VName
    | Let of VName * Expr * Expr
    | Print of Expr

type Program = Expr list
