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
    .>>. speciesLiteral
    .>> symbol "]"
    |>> Statements.ConcentrationStmt
    
// Step parser
let rec ifGT () =
    symbol "ifGT["
    >>. symbol "{"
    >>. many command
    .>> symbol "}"
    .>> symbol "]"
    |>> ConditionalStmt.IfGreaterThan

and conditionalStmt = ifGT() |>> Command.ConditionalStmt
and moduleStmt = ifGT() |>> Command.ConditionalStmt // TODO
    
and command = (conditionalStmt <|> moduleStmt) .>> skipComma
    
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
