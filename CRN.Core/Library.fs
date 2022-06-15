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
let skipComma = symbol "," |> skipMany1

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
let moduleStmt2SpeciesMaker id stmt =
    symbol $"{id}["
    >>. speciesLiteral
    .>> symbol ","
    .>>. speciesLiteral
    .>> symbol "]"
    |>> stmt

let moduleStmt3SpeciesMaker id stmt =
    symbol $"{id}["
    >>. speciesLiteral
    .>> symbol ","
    .>>. speciesLiteral
    .>> symbol ","
    .>>. speciesLiteral
    .>> symbol "]"
    |>> fun ((a, b), c) -> a, b, c
    |>> stmt
    
let load = moduleStmt2SpeciesMaker "ld" ModuleStmt.Load
let sqrt = moduleStmt2SpeciesMaker "sqrt" ModuleStmt.SquareRoot
let cmp = moduleStmt2SpeciesMaker "cmp" ModuleStmt.Compare
let add = moduleStmt3SpeciesMaker "add" ModuleStmt.Add
let sub = moduleStmt3SpeciesMaker "sub" ModuleStmt.Subtract
let mul = moduleStmt3SpeciesMaker "mul" ModuleStmt.Multiply
let div = moduleStmt3SpeciesMaker "div" ModuleStmt.Divide

let moduleStmt = choice [ load; sqrt; cmp; add; sub; mul; div ] |>> Command.ModuleStmt

// Command parser, forward created for recursive usage
let command, commandRef = createParserForwardedToRef<Command, unit>()

// Conditional statement parsers
let conditionalStmtMaker id stmt =
    symbol $"{id}["
    >>. symbol "{"
    >>. many command
    .>> symbol "}"
    .>> symbol "]"
    |>> stmt
    
let ifGT = conditionalStmtMaker "ifGT" ConditionalStmt.IfGreaterThan
let ifGE = conditionalStmtMaker "ifGE" ConditionalStmt.IfGreaterThanOrEquals
let ifEQ = conditionalStmtMaker "ifEQ" ConditionalStmt.IfEquals
let ifLT = conditionalStmtMaker "ifLT" ConditionalStmt.IfLesserThan
let ifLE = conditionalStmtMaker "ifLE" ConditionalStmt.IfLesserThanOrEquals

let conditionalStmt = choice [ ifGT; ifGE; ifEQ; ifLT; ifLE ] |>> Command.ConditionalStmt

// Command parser, declare actual parser after all necessary parsers in between are defined
commandRef.Value <- (moduleStmt <|> conditionalStmt)

// Step parser
let step =
    symbol "step["
    >>. symbol "{"
    >>. many (command .>> (attempt skipComma <|> skipString "}"))
    .>> symbol "]"
    |>> Statements.StepStmt
    
// Statement parser
let statement = (concentration <|> step)

// Full program parser
let program = symbol "crn"
              >>. symbol "="
              >>. symbol "{"
              >>. many (statement .>> (attempt skipComma <|> skipString "};"))
              |>> fun p -> { Statements = p }
              
let programFull = ws >>. program .>> ws .>> eof

// Parser for external use
let parse input =
    match run programFull input with
    | Success(res, _, _) -> Result.Ok res
    | Failure(err, _, _) -> Result.Error err