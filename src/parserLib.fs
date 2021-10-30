module ParserLib

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


// // type used when parsing binary ops
// type BinOp = (Expr -> Expr -> Expr)




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



let manyUntil p (until:char) =
    let label = sprintf "Many %s until %c" p.label until
    let rec inner acc input =
        match runOnInput p input with
            | Ok (r, rem) when r = until   -> Ok (List.rev acc, rem)
            | Ok (r, rem) -> inner (r::acc) rem            
            | Error e -> Error e

    {parseFun=(inner []); label=label}


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

