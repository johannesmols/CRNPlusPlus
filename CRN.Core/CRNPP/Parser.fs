module CRN.Core.CRNPP.Parser

open CRN.Core.CRNPP.Types
open CRN.Core.CRNPP.SemanticAnalyzer

open FParsec

// Basic parsers
let ws : Parser<_, unit> = spaces
let token p = p .>> ws
let symbol s = pstring s |> token
let skipComma = symbol "," |> skipMany1

// Literal parsers
let floatLiteral = pfloat |>> Literal.FloatLiteral .>> ws

let speciesLiteral = many1Chars (asciiLetter <|> digit) |>> Literal.SpeciesLiteral |> token

// Concentration statement parser
let concentration =
    symbol "conc["
    >>. speciesLiteral
    .>> symbol ","
    .>>. (floatLiteral <|> speciesLiteral)
    .>> symbol "]"
    |>> Statement.ConcentrationStmt
    
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

// Step parser
let step =
    symbol "step["
    >>. symbol "{"
    >>. many (command .>> (attempt skipComma <|> (symbol "}" |>> ignore)))
    .>> symbol "]"
    |>> Statement.StepStmt
    
// Command parser, declare actual parser after all necessary parsers in between are defined
commandRef.Value <- (moduleStmt <|> conditionalStmt)
    
// Statement parser
let statement = (concentration <|> step)

// Full program parser
let program = symbol "crn"
              >>. symbol "="
              >>. symbol "{"
              >>. many (statement .>> (attempt skipComma <|> (symbol "};" |>> ignore)))
              |>> fun p -> { Statements = p; Arguments = [] }
              
let programFull = ws >>. program .>> ws .>> eof

// Parser for external use
let parse input =
    match run programFull input with
    | Success(res, _, _) -> analyze res
    | Failure(err, _, _) -> Result.Error err