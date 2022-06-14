module CRN.Core

open FParsec

type Literal =
    | SpeciesLiteral of string
    | IntLiteral of int
    | FloatLiteral of float

type ModuleStmt =
    | Load of Literal * Literal
    | Add of Literal * Literal * Literal
    | Subtract of Literal * Literal * Literal
    | Multiply of Literal * Literal * Literal
    | Divide of Literal * Literal * Literal
    | SquareRoot of Literal * Literal
    | Compare of Literal * Literal
    
type ConditionalStmt =
    | IfGreaterThan of Command list
    | IfGreaterThanOrEquals of Command list
    | IfEquals of Command list
    | IfLesserThan of Command list
    | IfLesserThanOrEquals of Command list
    
and Command =
    | ArithmeticStmt // no definition ??
    | ComparisonStmt // no definition ??
    | ConditionalStmt of ConditionalStmt
    | ModuleStmt of ModuleStmt

type Statements =
    | ConcentrationStmt of Literal * Literal
    | StepStmt of Command list 

type Crn = {
    Statements : Statements list
}

// Basic parsers
let ws : Parser<_, unit> = spaces
let token p = p .>> ws
let symbol s = pstring s |> token
let skipComma = symbol "," |> skipMany

// Literal parsers
let intOrFloatLiteral =
    numberLiteral (NumberLiteralOptions.DefaultFloat ||| NumberLiteralOptions.DefaultInteger) "number"
    |>> fun n ->
            if n.IsInteger then Literal.IntLiteral (int n.String)
            else Literal.FloatLiteral (float n.String)
    .>> ws

let speciesLiteral = many1Chars (letter <|> digit) |>> Literal.SpeciesLiteral |> token

// Concentration statement parser
let concentration =
    symbol "conc["
    >>. speciesLiteral
    .>> symbol ","
    .>>. (intOrFloatLiteral <|> speciesLiteral)
    .>> symbol "]"
    |>> Statements.ConcentrationStmt
    
// Module statement parsers
let moduleStmt2Species id stmt =
    symbol $"{id}["
    >>. speciesLiteral
    .>> symbol ","
    .>>. speciesLiteral
    .>> symbol "]"
    |>> stmt

let moduleStmt3Species id stmt =
    symbol $"{id}["
    >>. speciesLiteral
    .>> symbol ","
    .>>. speciesLiteral
    .>> symbol ","
    .>>. speciesLiteral
    .>> symbol "]"
    |>> fun ((a, b), c) -> a, b, c
    |>> stmt
    
let load = moduleStmt2Species "ld" ModuleStmt.Load
let sqrt = moduleStmt2Species "sqrt" ModuleStmt.SquareRoot
let cmp = moduleStmt2Species "cmp" ModuleStmt.Compare
let add = moduleStmt3Species "sub" ModuleStmt.Add
let sub = moduleStmt3Species "sub" ModuleStmt.Subtract
let mul = moduleStmt3Species "sub" ModuleStmt.Multiply
let div = moduleStmt3Species "div" ModuleStmt.Divide

//and conditionalStmt = ifGT() |>> Command.ConditionalStmt
let moduleStmt = choice [ load; sqrt; cmp; add; sub; mul; div ]
                 |>> Command.ModuleStmt

let command = moduleStmt .>> skipComma
    
let step =
    symbol "step["
    >>. symbol "{"
    >>. many command
    .>> symbol "}"
    .>> symbol "]"
    |>> Statements.StepStmt
    
let statement = (concentration <|> step) .>> skipComma

// Full program parser
let program = symbol "crn"
              >>. symbol "="
              >>. symbol "{"
              >>. many statement
              .>> symbol "};"
              
let programFull = ws >>. program .>> ws .>> eof
