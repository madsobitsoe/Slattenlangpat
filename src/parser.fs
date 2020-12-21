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


type Position = {
    line : int
    column : int
    }

let initPos = {line=0; column=0}
let incrCol pos = { pos with column=pos.column + 1 }
let incrLine pos = { pos with line=pos.line + 1 }

type InputState = {
    lines : string[]
    position : Position
    }

// Inputstate from string
let inputStateFromString s =
    if String.IsNullOrEmpty(s) then
        {lines = [||]; position=initPos }
    else
        let seps = [| "\r\n"; "\n" |]
        let lines = s.Split(seps, StringSplitOptions.None)
        {lines=lines; position=initPos}

let getCurrentLine inputState =
    let linePos = inputState.position.line
    if linePos < inputState.lines.Length then
        inputState.lines.[linePos]
    else
        "EOF"

let nextChar inputState =
    let lPos = inputState.position.line
    let cPos = inputState.position.column
    // Next line is outside array of lines
    if lPos >= inputState.lines.Length then
        inputState, None
    //
    else
        let cl = getCurrentLine inputState
        // if next char is in the current line
        if cPos < cl.Length then
            let c = Some cl.[cPos]
            let nextPos = incrCol inputState.position
            let nextState = { inputState with position = nextPos }
            nextState, c
        // End of line, return newline and move to next line
        else
            let c = Some '\n'
            let nextPos = incrLine inputState.position
            let nextState = { inputState with position = nextPos }
            nextState, c


type ParserLabel = string
type ParserError = string

type ParserPosition = {
    currentLine : string
    line : int
    column : int
    }
type ParserFail = ParserLabel * ParserError * ParserPosition


let parserPositionFromInputState (inputState: InputState) = {
        currentLine = getCurrentLine inputState
        line = inputState.position.line
        column = inputState.position.column
    }

type Parser<'T> = {
    parseFun : (InputState -> Result<'T, ParserFail>)
    label : ParserLabel
    }




// Update the internal label in a parser
let setLabel parser newLabel =
    let inner input =
        let res = parser.parseFun input
        match res with
            | Ok res -> Ok res
            | Error (oldLabel, err, pos) ->
                Error (newLabel,err, pos)
    {parseFun=inner; label=newLabel}

// Infix version of setLabel
let ( <?> ) = setLabel
// Piping-version of setLabel
let ( |?> ) p l = setLabel p l

let getLabel parser =
    parser.label

let parseResultToString = function
    | Ok (value,input) ->
        sprintf "%A" value
    | Error (label:ParserLabel,error:ParserError, pos:ParserPosition) ->
        let errLine = pos.currentLine
        let cPos = pos.column
        let lPos = pos.line
        let failureCaret = sprintf "%*s^%s" cPos "" error
        sprintf "Line:%i Col:%i Error parsing %s\n%s\n%s" lPos cPos label errLine failureCaret
        // sprintf "Error parsing %s\n%s" label error

// Run parser on inputState
let runOnInput parser input = parser.parseFun input

// Run parser on string-input
let run parser input = runOnInput parser (inputStateFromString input)


let returnP x =
    let inner input =
        Ok  (x, input)
    {parseFun=inner; label="unknown"}


let bindP f p =
    let label = "unknown"
    let inner input =
        let res = runOnInput p input
        match res with
            | Error err -> Error err
            | Ok (res1,rem1) ->
                  let p2 = f res1
                  runOnInput p2 rem1
    {parseFun=inner; label=label}

// Infix version of BindP
let ( >>= ) p f = bindP f p


// type used when parsing binary ops
type BinOp = (Expr -> Expr -> Expr)




// Create a forward reference to a dummy parser, to handle mutually recursive parsers
let createParserForwardedToRef<'T>() =
    let dummy =
        let inner (input:InputState) : Result<'T * InputState,ParserFail> = failwith "unfixed forwarded parser"
        {parseFun=inner; label="DUMMY"}
    // ref cell to dummy parser
    let parserRef = ref dummy
    // Wrapper parser
    let inner input =
        runOnInput !parserRef input
    let wrapper = {parseFun=inner;label="unknown"}
    (wrapper, parserRef)

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


let mapP f p =
    let inner input =
        match runOnInput p input with
        | Error err -> Error err
        | Ok (res,rem) ->
            Ok (f res, rem)
    {parseFun=inner; label=p.label}

let (<!>) = mapP
// "piping" operator for mapP
let ( |>> ) x f = f <!> x


let andThen p1 p2 =
    let label = (getLabel p1,getLabel p2) ||> sprintf "%s andThen %s"
    p1 >>= (fun p1Res ->
    p2 >>= (fun p2Res ->
        returnP (p1Res,p2Res)
    ))
    <?> label // set the new label to fit both parsers

let  (.>>.) = andThen
// Keep left side
let (.>>) p1 p2 =
    p1 .>>. p2 |> mapP (fun (a,b) -> a)

// Keep right side
let (>>.) p1 p2 =
    p1 .>>. p2 |> mapP (fun (a,b) -> b)

let orElse p1 p2 =
    let label = (getLabel p1, getLabel p2) ||> sprintf "%s or %s"
    let inner input =
        match runOnInput p1 input with
            | Ok res -> Ok res
            | Error err1 ->
                runOnInput p2 input
    {parseFun=inner; label=label}

// infix version of orElse
let (<|>) = orElse


let choice parsers =
    match parsers with
        | [] -> failwith "No parsers for choice. List was empty"
        | _ -> List.reduce ( <|> ) parsers

let anyOf chars =
    let label = sprintf "any of %A" chars
    chars
    |> List.map pChar
    |> choice
    <?> label




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
    match runOnInput p input with
        | Error _ -> ([], input)
        | Ok (res,rem) ->
            let res1,rem1 = pZeroOrMore p rem
            (res::res1,rem1)

let many p =
    let label = sprintf "many of %s" <| p.label
    let pfun = (fun input -> Ok <| pZeroOrMore p input)
    {parseFun=pfun; label=label}


let many1 p =
    let label = sprintf "one or more of %s" <| p.label
    let inner input =
        match runOnInput p input with
        | Error e -> Error e
        | Ok (r,rem) ->
            let (rs,rem1) = pZeroOrMore p rem
            Ok (r::rs, rem1)

    {parseFun=inner; label=label}

let skipMany p =
    {parseFun=(fun input ->
                let (_,rem) = pZeroOrMore p input
                Ok ((),rem)
            ); label=p.label}

let skipMany1 p =
    let label = sprintf "one or more of %s" <| p.label
    let inner input =
        match runOnInput p input with
            | Error e -> Error e
            | Ok (_,rem) ->
                let (_,rem') = pZeroOrMore p rem
                Ok ((), rem')
    {parseFun=inner; label=label}

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
    let label = sprintf "one or more of %s separated by a binop" <| (p.label)
    let inner input =
        let parseSingle = runOnInput p input
        let rec parsePair p op prev =
            match prev with
                | Error err -> Error err
                | Ok (res,rem) ->
                    match runOnInput op rem with
                        | Error _ -> Ok (res,rem)
                        | Ok (pOp,rem') ->
                            match runOnInput p rem' with
                                | Error err -> Error err
                                | Ok (res',rem'') ->
                                    parsePair p op (Ok (pOp res res', rem''))
        parseSingle |> parsePair p op

    {parseFun=inner; label=label}


// The parser chainr1 p op parses one or more occurrences of p separated by op
// right-associative
// Most probably not the optimal way to implement this
let chainr1 p op =
    let label = sprintf "one or more of %s separated by a binop" <| (p.label)
    let rec inner input =
        match runOnInput p input with
            | Error err -> Error err
            | Ok (res,rem) ->
                match runOnInput op rem with
                    | Error err -> Ok (res,rem)
                    | Ok (opF,rem') ->
                        match inner rem' with
                            | Error err -> Error err
                            | Ok (res',rem'') -> Ok  (opF res res', rem'')
    {parseFun=inner; label=label}


// --------------------------------
// SLP-parsers
// --------------------------------

let whitespace = satisfy (fun x -> List.contains x [' ';'\n';'\t']) "space, newline or tab"
let separator = skipMany whitespace <?> "separator"
let pString : string -> Parser<string * InputState> =
    List.ofSeq >> List.map pChar >> sequence >> mapP csToS

// A keyword should be followed by whitespace
let keywords = ["print";"let";"in"]
// Use with another parser, like `keyword (pString "print")
let keyword p =
    p .>> separator

let pDigit = satisfy Char.IsDigit "digit" //anyOf ['0'..'9']

// compare char to ascii-range 'A'..'Z' @ 'a'..'z'
let isLetter : char -> bool = (int >> fun x ->
                               (0x41 <= x && x <= 0x5a) ||
                               (0x61 <= x && x <= 0x7a))
let pLetter = satisfy isLetter "letter"
let pLetterOrDigit = (pLetter <|> pDigit) <?> "letter or digit"
// Parses an operator, eats ws on both sides
let pOp op = (many whitespace >>. pChar op .>> many whitespace) <?> string op
let pPlus = pOp '+'
let pMinus = pOp '-'
let pEquals = pOp '='
let pLParen = pOp '('
let pRParen = pOp ')'

let betweenParen p = (between pLParen p pRParen) |?> sprintf "( %A )" p.label

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


let pConst = many1 pDigit .>> skipMany whitespace |>>  (csToS >> int >> fun x -> Const (Int x))
let pVar = pIdent .>> skipMany whitespace |>> Var
let pBinOp =
    let inner input =
        match runOnInput pPlus input with
            | Ok (_,rem) -> Ok ( (fun a b -> Add (a,b)), rem)
            | Error err ->
                match runOnInput pMinus input with
                    | Ok (_,rem) -> Ok ((fun a b -> Sub (a,b)), rem)
                    | Error err ->  Error err
    {parseFun=inner; label="expression(s separated by binary ops)"}


// Deal with F# stupid non-lazyness and freaky (sane) rules for mutually recursive definitions
// In haskell this would just work, grr.
// Define a parser, that dereferences a reference to another parser and uses that.
// The parser, pExpr, is defined here, so it can can be used by pExprT2 and pExpr T1.
// The actual parser, i.e. the reference, is set up after the parsers that need to know about it.
let pExpr,pExprRef =
    let pExpr,pExprRef = createParserForwardedToRef<Expr>()
    setLabel pExpr "expression",pExprRef

// Let-expressions contain expressions, so the parser should be defined after the expression parser.
let pLetExpr =
    let f name e1 e2 = Let (name,e1,e2)
    let idP = keyword (pString "let") >>. pIdent .>> (skipMany whitespace .>>. pEquals .>>. skipMany whitespace)
    let e1P = pExpr .>> separator
    let e2P = keyword (pString "in") >>. pExpr .>> many whitespace
    // lift f to Parser<f> and apply it to the 3 subparsers of the let-expr.
    // thus creating a Parser<Expr>
    returnP f <*> idP <*> e1P <*> e2P
    <?> "let IDENTIFIER = EXPR1 in EXPR2"
// Generate Print expressions
let pPrintExpr =
    keyword (pString "print") >>. pExpr |> mapP (fun x -> Print x)
    <?> "print EXPR"


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
    let inputState = inputStateFromString program
    match runOnInput pExpr inputState with
        | Ok (res,state) ->
            match getCurrentLine state with
                | "EOF" -> Ok res
                | rem -> sprintf "Did not consume all input. Left: %s..." rem |> Error
        | Error err -> Error (parseResultToString (Error err))
