module Parser
open AST


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


let pChar charToMatch =
    let innerParser (s:string) =
        if s.Length = 0 then Error "EOF"
        else
            if charToMatch = s.[0] then Ok (charToMatch, s.[1..])
            else Error <| sprintf "Expecting %c, got %c" charToMatch s.[0]
    Parser innerParser

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




// The parser chainl1 p op parses one or more occurrences of p separated by op
// left-associative
// Most probably not the optimal way to implement this
let chainl1 p op =
    let inner input =
        let parseSingle = run p input
        let rec parsePair p op prev =
            printfn "called! %A" prev
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
// SLP-parsers, types and definitions
// --------------------------------

let ws = let wsChars = anyOf [' ';'\n';'\t'] in many1 wsChars


let pString : string -> Parser<string * string> = List.ofSeq >> List.map pChar >> sequence >> mapP csToS


let pDigit = anyOf ['0'..'9']


let pPlus = pChar '+'
let pMinus = pChar '-'
let pParen = pChar '('
let pCParen = pChar ')'

let betweenParen p = between pParen p pCParen



let pConst = many1 pDigit |> mapP (csToS >> int >> Const)
let pAdd = sepBy pConst pPlus  |>> List.reduce (fun a b ->  Add (a,b))
let pSub = sepBy pConst pMinus |>> List.reduce (fun a b ->  Sub (a,b))
let pBinOp =
    let inner input =
        match run pPlus input with
            | Ok (_,rem) -> Ok ( (fun a b -> Add (a,b)), rem)
            | Error err ->
                match run pMinus input with
                    | Ok (_,rem) -> Ok ((fun a b -> Sub (a,b)), rem)
                    | Error err ->  Error err
    Parser inner

let pExpr = chainl1 pConst pBinOp
// Not used, but might be useful at some point
let pExprRightAssoc = chainr1 pConst pBinOp

// Various "tests" of chainl and chainr
// run pExpr   "1"
// run pExpr "1+2"
// run pExpr "1+2+3"
// run pExpr "1-2-3-4"
// run pExpr "1+2+3+4-5+6-7-8-1+90"

// run pExprRightAssoc   "1"
// run pExprRightAssoc "1+2"
// run pExprRightAssoc "1+2+3"
// run pExprRightAssoc "1-2-3-4"
// run pExprRightAssoc "1+2+3+4-5+6-7-8-1+90"


let parse (program:string) : Result<Expr,string> =
    match run pExpr program with
        | Ok (res:Expr,rem:string) -> Ok res
        | Error err -> Error err
