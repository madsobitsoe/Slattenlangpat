module Parser
open AST
// only for String.IsNullOrEmpty
open System

// --------------------------------
// helper functions
// --------------------------------

// chars to string
let (csToS : char list -> string) = List.fold (fun acc x -> string x |> (+) acc ) ""



// --------------------------------
// library part
// --------------------------------

type Parser<'T> = Parser of (string -> Result<'T, string>)
type BinOp = (Expr -> Expr -> Expr)

let run parser input =
    let (Parser parse) = parser
    parse input



// Create a forward reference to a dummy parser, to handle mutually recursive parsers
let createParserForwardedToRef<'T>() =
    let dummy =
        let inner input : Result<'T,string> = failwith "unfixed forwarded parser"
        Parser inner
    // ref cell to dummy parser
    let parserRef = ref dummy
    // Wrapper parser
    let inner input =
        run !parserRef input
    (Parser inner, parserRef)

let satisfy predicate =
    let inner input =
        if String.IsNullOrEmpty(input) then
            Error "No more input"
        else
            let first = input.[0]
            if predicate first then
                let rem = input.[1..]
                Ok (first,rem)
            else
                Error <| sprintf "Unexpected '%c'" first
    Parser inner

let pChar c =
    (=) c
    |> satisfy


let mapP f p =
    let inner input =
        match run p input with
        | Error e -> Error e
        | Ok (res,rem) ->
            Ok (f res, rem)
    Parser inner

let (<!>) = mapP

let ( |>> ) x f = f <!> x

let (.>>.) p1 p2 =
    let inner input =
        match run p1 input with
            | Error err1 -> Error err1
            | Ok (res1,rem1) ->
                match run p2 rem1 with
                    | Error err2 -> Error err2
                    | Ok (res2,rem2) ->
                        Ok ((res1,res2),rem2)
    Parser inner


// Keep left side
let (.>>) p1 p2 =
    p1 .>>. p2 |> mapP (fun (a,b) -> a)

// Keep right side
let (>>.) p1 p2 =
    p1 .>>. p2 |> mapP (fun (a,b) -> b)

// p1 or p2
let (<|>) p1 p2 =
    let inner input =
        let res1 = run p1 input
        match run p1 input with
            | Ok res -> Ok res
            | Error err1 ->
                run p2 input
    Parser inner


let choice parsers =
    match parsers with
        | [] -> failwith "No parsers for choice. List was empty"
        | _ -> List.reduce ( <|> ) parsers

let anyOf chars =
    List.map pChar chars
    |> choice


let returnP x =
    let inner input =
        Ok  (x, input)
    Parser inner


let applyP fP xP =
    (fun (f,x) -> f x) <!> (fP .>>. xP)

let (<*>) = applyP

let lift2 f xP yP =
    returnP f <*> xP <*> yP


let rec sequence parsers =
    let cons x xs = x :: xs
    let consP = lift2 cons
    match parsers with
        | [] -> returnP []
        | x::xs -> consP x (sequence xs)


let rec pZeroOrMore p input =
    match run p input with
        | Error _ -> ([], input)
        | Ok (res,rem) ->
            let res1,rem1 = pZeroOrMore p rem
            (res::res1,rem1)

let many p =
    Parser (fun input -> Ok <| pZeroOrMore p input)

let many1 p =
    let inner input =
        match run p input with
        | Error e -> Error e
        | Ok (r,rem) ->
            let (rs,rem1) = pZeroOrMore p rem
            Ok (r::rs, rem1)
    Parser inner

let skipMany p =
    Parser (fun input ->
                let (_,rem) = pZeroOrMore p input
                Ok ((),rem)
            )

let skipMany1 p =
    let inner input =
        match run p input with
            | Error e -> Error e
            | Ok (_,rem) ->
                let (_,rem') = pZeroOrMore p rem
                Ok ((), rem')
    Parser inner

let between p1 p2 p3 =
    p1 >>. p2 .>> p3

let opt p =
    let some = p |>> Some
    let none = returnP None
    some <|> none


let sepBy1 p sep =
    p .>>. many (sep >>. p)
    |>> (fun (p,ps) ->  p::ps)

let sepBy p sep = sepBy1 p sep <|> returnP []

// How fParsec does it
// let chainl1 p op =
//     Inline.SepBy((fun x0 -> x0), (fun x f y -> f x y), (fun x -> x), p, op)

// let chainl p op x = chainl1 p op <|>% x

// The parser chainl1 p op parses one or more occurrences of p separated by op
// left-associative
// Definitely not the optimal way to implement this. This is slow AF
let chainl1 p op =
    let inner input =
        let parseSingle = run p input
        let rec parsePair p op prev =
            match prev with
                | Error err -> Error err
                | Ok (res,rem) ->
                    match run op rem with
                        | Error _ -> Ok (res,rem)
                        | Ok (pOp,rem') ->
                            match run p rem' with
                                | Error err -> Error err
                                | Ok (res',rem'') ->
                                    parsePair p op (Ok (pOp res res', rem''))
        parseSingle |> parsePair p op

    Parser inner


// The parser chainr1 p op parses one or more occurrences of p separated by op
// right-associative
// Most probably not the optimal way to implement this
let chainr1 p op =
    let rec inner input =
        match run p input with
            | Error err -> Error err
            | Ok (res,rem) ->
                match run op rem with
                    | Error err -> Ok (res,rem)
                    | Ok (opF,rem') ->
                        match inner rem' with
                            | Error err -> Error err
                            | Ok (res',rem'') -> Ok  (opF res res', rem'')
    Parser inner


// --------------------------------
// SLP-parsers
// --------------------------------

let whitespace = satisfy (fun x -> List.contains x [' ';'\n';'\t'])
let separator = skipMany whitespace
let pString : string -> Parser<string * string> =
    List.ofSeq >> List.map pChar >> sequence >> mapP csToS

// A keyword should be followed by whitespace
let keywords = ["print";"let";"in"]
// Use with another parser, like `keyword (pString "print")
let keyword p =
    p .>> separator

let pDigit = satisfy Char.IsDigit //anyOf ['0'..'9']

// compare char to ascii-range 'A'..'Z' @ 'a'..'z'
let isLetter : char -> bool = (int >> fun x ->
                               (0x41 <= x && x <= 0x5a) ||
                               (0x61 <= x && x <= 0x7a))
let pLetter = satisfy isLetter
let isLetterOrDigit = pLetter <|> pDigit
let pPlus = many whitespace >>. pChar '+' .>> many whitespace
let pMinus = many whitespace >>. pChar '-' .>> many whitespace
let pEquals = many whitespace >>. pChar '=' .>> many whitespace
let pLParen = many whitespace >>. pChar '(' .>> many whitespace
let pRParen = many whitespace >>. pChar ')' .>> many whitespace

let betweenParen p = between pLParen p pRParen

// Parse an identifier, i.e. VName
let pIdent =
    let inner input =
        let p = pLetter .>>. many isLetterOrDigit
        match run p input with
            | Error err -> Error err
            | Ok ((h,t),rem) ->
                let sRes = csToS (h::t)
                if List.contains sRes keywords then sprintf "%s is a reserved keyword. It cannot be used as a name." sRes |> Error
                else Ok ((sRes:VName), rem)
    Parser inner


let pConst = many1 pDigit .>> skipMany whitespace |>>  (csToS >> int >> fun x -> Const (Int x))
let pVar = pIdent .>> skipMany whitespace |>> Var
let pBinOp =
    let inner input =
        match run pPlus input with
            | Ok (_,rem) -> Ok ( (fun a b -> Add (a,b)), rem)
            | Error err ->
                match run pMinus input with
                    | Ok (_,rem) -> Ok ((fun a b -> Sub (a,b)), rem)
                    | Error err ->  Error err
    Parser inner


// Deal with F# stupid non-lazyness and freaky (sane) rules for mutually recursive definitions
// In haskell this would just work, grr.
// Define a parser, that dereferences a reference to another parser and uses that.
// The parser, pExpr, is defined here, so it can can be used by pExprT2 and pExpr T1.
// The actual parser, i.e. the reference, is set up after the parsers that need to know about it.
let pExpr,pExprRef = createParserForwardedToRef<Expr*string>()

// Let-expressions contain expressions, so the parser should be defined after the expression parser.
let pLetExpr =
    let f name e1 e2 = Let (name,e1,e2)
    let idP = keyword (pString "let") >>. pIdent .>> (skipMany whitespace .>>. pEquals .>>. skipMany whitespace)
    let e1P = pExpr .>> separator
    let e2P = keyword (pString "in") >>. pExpr .>> many whitespace
    // lift f to Parser<f> and apply it to the 3 subparsers of the let-expr.
    // thus creating a Parser<Expr>
    returnP f <*> idP <*> e1P <*> e2P

// Generate Print expressions
let pPrintExpr =
    keyword (pString "print") >>. pExpr |> mapP (fun x -> Print x)


let pExprT2 =
    betweenParen pExpr
let pExprT1 =
    pConst
    <|> pVar
    <|> pLetExpr
    <|> pPrintExpr
    <|> pExprT2

// Set up the actual top-level parser, so it can be used recursively by sub-parsers
pExprRef := chainl1 pExprT1 pBinOp

let parse (program:string) : Result<Expr,string> =
    match run pExpr program with
        | Ok (res:Expr,rem:string) ->
            if not (String.IsNullOrEmpty(rem)) then sprintf "Did not consume all input. Left: %s" rem |> Error
            else Ok res
        | Error err -> Error err
