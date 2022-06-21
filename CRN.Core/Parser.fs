// Johannes Mols, 15-06-2022: Entire parser
// Patrikas Balsys, 20-06-2022: Added RXN statement parsing

module CRN.Core.Parser

open CRN.Core.Types.Parser
open CRN.Core.SemanticAnalyzer

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
    symbol "conc"
    >>. symbol "["
    >>. speciesLiteral
    .>> symbol ","
    .>>. (floatLiteral <|> speciesLiteral)
    .>> symbol "]"
    |>> Statement.ConcentrationStmt
    
// Module statement parsers
let moduleStmt2SpeciesMaker id stmt =
    symbol $"{id}"
    >>. symbol "["
    >>. speciesLiteral
    .>> symbol ","
    .>>. speciesLiteral
    .>> symbol "]"
    |>> stmt

let moduleStmt3SpeciesMaker id stmt =
    symbol id
    >>. symbol "["
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
    symbol id
    >>. symbol "["
    >>. symbol "{"
    >>. many (command .>> (attempt skipComma <|> (symbol "}" |>> ignore)))
    .>> symbol "]"
    |>> stmt
    
let ifGT = conditionalStmtMaker "ifGT" ConditionalStmt.IfGreaterThan
let ifGE = conditionalStmtMaker "ifGE" ConditionalStmt.IfGreaterThanOrEquals
let ifEQ = conditionalStmtMaker "ifEQ" ConditionalStmt.IfEquals
let ifLT = conditionalStmtMaker "ifLT" ConditionalStmt.IfLesserThan
let ifLE = conditionalStmtMaker "ifLE" ConditionalStmt.IfLesserThanOrEquals

let conditionalStmt = choice [ ifGT; ifGE; ifEQ; ifLT; ifLE ] |>> Command.ConditionalStmt

// Reaction statement parser
let expression = sepBy1 speciesLiteral (symbol "+")

let reactionStmt =
    symbol "rxn"
    >>. symbol "["
    >>. expression
    .>> symbol ","
    .>>. expression
    .>> symbol ","
    .>>. floatLiteral
    .>> symbol "]"
    |>> (fun ((a, b), c) -> a, b, c)
    |>> Reaction
    |>> ReactionStmt

// Step parser
let step =
    symbol "step"
    >>. symbol "["
    >>. symbol "{"
    >>. many (command .>> (attempt skipComma <|> (symbol "}" |>> ignore)))
    .>> symbol "]"
    |>> Statement.StepStmt
    
// Command parser, declare actual parser after all necessary parsers in between are defined
commandRef.Value <- (moduleStmt <|> conditionalStmt <|> reactionStmt)
    
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
    | Success(res, _, _) -> Result.Ok res
    | Failure(err, _, _) -> Result.Error err