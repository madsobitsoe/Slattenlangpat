module AST

type Expr =
    | Const of int
    | Add of (Expr * Expr)
    | Sub of (Expr * Expr)
    | Print of Expr

type Program = Expr list
