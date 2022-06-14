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

let ws : Parser<_, unit> = skipMany (skipChar ' ')

let intOrFloatLiteral =
    numberLiteral (NumberLiteralOptions.DefaultFloat ||| NumberLiteralOptions.DefaultInteger) "number"
    |>> fun n ->
            if n.IsInteger then Literal.IntLiteral (int n.String)
            else Literal.FloatLiteral (float n.String)
    .>> ws

let speciesLiteral = many1Chars (letter <|> digit) |>> Literal.SpeciesLiteral .>> ws

let statements : Parser<_, unit> = spaces // TODO

let program = skipString "crn"
              >>. spaces
              >>. skipChar '='
              >>. spaces
              >>. skipChar '{'
              >>. spaces
              >>. statements
              >>. spaces
              >>. skipString "};" 
let programFull = spaces >>. program .>> spaces .>> eof
