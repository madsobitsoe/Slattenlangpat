module Parser
open AST
open ParserLib

// type used when parsing binary ops
type BinOp = (Expr -> Expr -> Expr)

// --------------------------------
// SLP-parsers
// --------------------------------
let satisfy predicate label =
    let inner input =
        let remInput,charOpt = nextChar input
        let pos = parserPositionFromInputState input
        match charOpt with
            | None ->
                let err = "No more input"
                Error (label,err,pos)
            | Some first ->
                if predicate first then
                    Ok (first,remInput)
                else
                    let err = sprintf "Unexpected '%c'" first
                    Error (label,err,pos)
    {parseFun=inner; label=label }

let pChar c =
    (=) c
    |> satisfy <| sprintf "%c" c


let anything =
    satisfy (fun _ -> true)  "literally anything"
    
let anyOf chars =
    let label = sprintf "any of %A" chars
    chars
    |> List.map pChar
    |> choice
    <?> label





let whitespace = satisfy (fun x -> List.contains x [' ';'\n';'\t']) "space, newline or tab"
let separator = skipMany whitespace <?> "separator"
let stmtSeparator = pChar ';' <?>  "Statement Separator" .>> separator



// A keyword should be followed by whitespace
let keywords = ["print";"let";"in";"fst";"snd"]
// Use with another parser, like `keyword (pString "print")
let keyword p =
    p .>> separator


let pString : string -> Parser<string * InputState> =
    List.ofSeq >> List.map pChar >> sequence >> mapP csToS


// Parses an operator, eats ws on both sides
let pOp op = (separator >>. pChar op .>> separator) <?> string op
// Used for let bindings
let pEquals = pOp '='
// Used for parenthesizing expressions
let pLParen = pOp '('
let pRParen = pOp ')'
let betweenParen p = (between pLParen p pRParen) |?> sprintf "( %A )" p.label

// Used for binary operators separating two expressions
let pPlus =  pChar '+' |>> (fun _ a b -> Oper (Plus, a, b))
let pMinus = pChar '.' |>> (fun _ a b -> Oper (Minus, a, b))
let pEQ = pChar '=' |>> (fun _ a b -> Oper (EQ, a, b))
let pLT = pChar '<' |>> (fun _ a b -> Oper (LT, a, b))
let pGT = pChar '>' |>> (fun _ a b -> Oper (GT, a, b))
let pLTE = keyword (pString "<=") |>> (fun _ a b -> Oper (LTE, a, b))
let pGTE = keyword (pString ">=") |>> (fun _ a b -> Oper (GTE, a, b))
let pNotEQ = keyword (pString "<>") |>> (fun _ a b -> Oper (NotEQ, a, b))


let manyChars cp =
    many cp |>> csToS

let manyChars1 cp =
    many1 cp |>> csToS

let pDQ = pChar '"'
// Parses an escaped newline in a string (not a literal newline)
let pNewLine = pString "\\n" |>> (fun _ -> '\n')

let pStringValue =
    pDQ >>. (manyUntil (pNewLine <|> anything) '"') |>> csToS |>> AST.String |>> Const
    <?> "string literal"


// Booleans
let pTrue = keyword (pString "true") |>> (fun _ -> Const (Bool true))
let pFalse = keyword (pString "false") |>> (fun _ -> Const (Bool false))
let pBool = pTrue <|> pFalse 


// Integers
let pDigit = satisfy System.Char.IsDigit "digit" //anyOf ['0'..'9']
let pInt =
    let toSignedInt (sign, digits) =
        let i = digits |> int //No overflow checking yet
        match sign with
            | Some _ -> -i
            | None -> i
    let digits = manyChars1 pDigit
    opt (pChar '-') .>>. digits .>> separator
    |>> toSignedInt |>> Int |>> Const
    <?> "integer"


// compare char to ascii-range 'A'..'Z' @ 'a'..'z'
let isLetter : char -> bool = (int >> fun x ->
                               (0x41 <= x && x <= 0x5a) ||
                               (0x61 <= x && x <= 0x7a))
let pLetter = satisfy isLetter "letter"
let pLetterOrDigit = (pLetter <|> pDigit) <?> "letter or digit"




// Parse an identifier, i.e. VName
// TODO FIX THIS WEIRD MESS
let pIdent =
    let inner input =
        let p = {
            parseFun=(pLetter .>>. many pLetterOrDigit).parseFun;
            label="letter followed by letters or digits"
            }
        match runOnInput p input with
            | Error err -> Error err
            | Ok ((h,t),rem) ->
                let sRes = csToS (h::t)
                if List.contains sRes keywords then
                    Error (p.label, sprintf "%s is a reserved keyword. It cannot be used as a name." sRes, parserPositionFromInputState input)
                else Ok ((sRes:VName), rem)
    {parseFun=inner; label="identifier"}

let pConst = (pBool <|> pStringValue <|> pInt) .>> separator

let pVar = pIdent .>> separator |>> Var


let pBinOp =
    (pPlus <|> pMinus <|> pEQ <|> pLTE <|> pGTE <|> pNotEQ <|> pLT <|> pGT )
    .>> separator


// Deal with F# stupid non-lazyness and freaky (sane) rules for mutually recursive definitions
// In haskell this would just work, grr.
// Define a parser, that dereferences a reference to another parser and uses that.
// The parser, pExpr, is defined here, so it can can be used by pExprT2 and pExpr T1.
// The actual parser, i.e. the reference, is set up after the parsers that need to know about it.
let pExpr,pExprRef =
    let pExpr,pExprRef = createParserForwardedToRef<Expr>()
    setLabel pExpr "expression",pExprRef

// Let-expressions contain expressions, so the parser should be defined after the expression parser.
// let pLetExpr =
//     let f name e1 e2 = Let (name,e1,e2)
//     let idP = keyword (pString "let") >>. pIdent .>> (skipMany whitespace .>>. pEquals .>>. skipMany whitespace)
//     let e1P = pExpr .>> separator
//     let e2P = keyword (pString "in") >>. pExpr .>> many whitespace
//     // lift f to Parser<f> and apply it to the 3 subparsers of the let-expr.
//     // thus creating a Parser<Expr>
//     returnP f <*> idP <*> e1P <*> e2P
//     <?> "let IDENTIFIER = EXPR1 in EXPR2"
// Generate Print expressions


let helperUnwrapConst : (Expr -> Value) = function
        | Const v -> v

let pPair =
    betweenParen (pConst .>> separator .>> pChar ',' .>>. pConst) .>> separator
    |>> (fun (l,r) -> Const (Pair (helperUnwrapConst l, helperUnwrapConst r)))
    // lParen >>. pExpr .>> separator .>> pChar ',' .>> separator .>>. pExpr .>> rparen

let pPrintExpr =
    keyword (pString "print") >>. pExpr |> mapP (fun x -> Call ("print", [x]))
    <?> "print EXPR"

let pFst =
    keyword (pString "fst") >>. pExpr |>> (fun x -> Call ("fst", [x]))
let pSnd =
    keyword (pString "snd") >>. pExpr |>> (fun x -> Call ("snd", [x]))

let pBuiltInCalls =
    pPrintExpr
    <|> pFst
    <|> pSnd

// Match cases
let pMatchCase =
    keyword (pChar '|') >>. pExpr .>> keyword (pString "->") .>> separator .>>. pExpr
    
let pMatchExpr =
    keyword (pString "match") >>. pExpr .>> separator .>> keyword (pString "with") .>>.
    many1 pMatchCase
    |>> (fun (me, mcs) -> Match (me, mcs))



// The (sub)-expression parsers
let pExprT2 =
    betweenParen pExpr
let pExprT1 =
    pMatchExpr
    <|> pPair
    <|> pConst
    <|> pVar
    <|> pBuiltInCalls
    <|> pExprT2

// Set up the actual top-level parser, so it can be used recursively by sub-parsers
pExprRef := chainl1 pExprT1 pBinOp


let pStatement =
    separator >>.
    (
    (keyword (pString "let") >>. pIdent .>> pEquals .>>. pExpr .>> stmtSeparator) |>> (fun (name,exp) -> SDef (name,exp))
    <|>
    (pExpr .>> stmtSeparator |>> SExp ))
    

let pProgram : Parser<Program * InputState> =
    many1 pStatement


let parse (program:string) : Result<Program,string> =
    let inputState = inputStateFromString program
    match runOnInput pProgram inputState with
        | Ok (res,state) ->
            match getCurrentLine state with
                | "EOF" -> Ok res
                | rem -> sprintf "Did not consume all input. Left: %s..." rem |> Error
        | Error err -> Error (parseResultToString (Error err))
