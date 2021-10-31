module AST


type Value = Bool of bool | Int of int | String of string | Unit | Pair of Value * Value
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



type BasicType = BoolT | IntT | StringT | UnitT | PairT of BasicType * BasicType | UndecidedT
type TypedExpr =
    | TypedConst of Value * BasicType
    | TypedVar of VName * BasicType
    | TypedOper of (Op * TypedExpr * TypedExpr) * BasicType
    | TypedCall of (VName * TypedExpr list) * BasicType
    | TypedPrint of TypedExpr * BasicType
    | TypedMatch of (TypedExpr * (TypedExpr * TypedExpr) list) * BasicType
    

type TypedStatement =
    | TypedSDef of VName * TypedExpr
    | TypedSExp of TypedExpr

type TypedProgram = TypedStatement list
