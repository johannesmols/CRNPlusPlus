#r "nuget: FParsec"

open FParsec

type Literal =
    | SpeciesLiteral of string
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
    | ConditionalStmt of ConditionalStmt
    | ModuleStmt of ModuleStmt

type Statements =
    | ConcentrationStmt of Literal * Literal
    | StepStmt of Command list

type Crn = { Statements: Statements list }

// Basic parsers
let ws: Parser<_, unit> = spaces
let token p = p .>> ws
let symbol s = pstring s |> token
let skipComma = symbol "," |> skipMany1

// Literal parsers
let floatLiteral = pfloat |>> FloatLiteral .>> ws

let speciesLiteral =
    many1Chars (letter <|> digit) |>> SpeciesLiteral
    |> token

// Concentration statement parser
let concentration =
    symbol "conc[" >>. speciesLiteral .>> symbol ","
    .>>. floatLiteral
    .>> symbol "]"
    |>> ConcentrationStmt

// Module statement parsers
let moduleStmt2SpeciesMaker id stmt =
    symbol $"{id}[" >>. speciesLiteral .>> symbol ","
    .>>. speciesLiteral
    .>> symbol "]"
    |>> stmt

let moduleStmt3SpeciesMaker id stmt =
    symbol $"{id}[" >>. speciesLiteral .>> symbol ","
    .>>. speciesLiteral
    .>> symbol ","
    .>>. speciesLiteral
    .>> symbol "]"
    |>> fun ((a, b), c) -> a, b, c
    |>> stmt

let load = moduleStmt2SpeciesMaker "ld" Load
let sqrt = moduleStmt2SpeciesMaker "sqrt" SquareRoot
let cmp = moduleStmt2SpeciesMaker "cmp" Compare
let add = moduleStmt3SpeciesMaker "add" Add
let sub = moduleStmt3SpeciesMaker "sub" Subtract
let mul = moduleStmt3SpeciesMaker "mul" Multiply
let div = moduleStmt3SpeciesMaker "div" Divide

let moduleStmt =
    choice [ load
             sqrt
             cmp
             add
             sub
             mul
             div ]
    |>> ModuleStmt

// Command parser, forward created for recursive usage
let command, commandRef = createParserForwardedToRef<Command, unit> ()

// Conditional statement parsers
let conditionalStmtMaker id stmt =
    symbol $"{id}[" >>. symbol "{" >>. many command
    .>> symbol "}"
    .>> symbol "]"
    |>> stmt

let ifGT = conditionalStmtMaker "ifGT" IfGreaterThan
let ifGE = conditionalStmtMaker "ifGE" IfGreaterThanOrEquals
let ifEQ = conditionalStmtMaker "ifEQ" IfEquals
let ifLT = conditionalStmtMaker "ifLT" IfLesserThan
let ifLE = conditionalStmtMaker "ifLE" IfLesserThanOrEquals

let conditionalStmt =
    choice [ ifGT; ifGE; ifEQ; ifLT; ifLE ]
    |>> ConditionalStmt

// Step parser
let step =
    symbol "step["
    >>. symbol "{"
    >>. many (
        command
        .>> (attempt skipComma <|> (symbol "}" |>> ignore))
    )
    .>> symbol "]"
    |>> Statements.StepStmt

// Command parser, declare actual parser after all necessary parsers in between are defined
commandRef.Value <- (moduleStmt <|> conditionalStmt)

// Statement parser
let statement = (concentration <|> step)

// Full program parser
let program =
    symbol "crn"
    >>. symbol "="
    >>. symbol "{"
    >>. many (
        statement
        .>> (attempt skipComma <|> (symbol "};" |>> ignore))
    )
    |>> fun p -> { Statements = p }

let programFull = ws >>. program .>> ws .>> eof

// Parser for external use
let parse input =
    match run programFull input with
    | Success (res, _, _) -> Result.Ok res
    | Failure (err, _, _) -> Result.Error err


//* -------------

let crn1 =
    """
crn = {
    conc[c,5], conc[ cInitial ,5],
    conc[one ,1], conc[zero ,0],
    step[{
        sub[c,one,cnext ],
        cmp[c,zero]
    }],
    step[{
        ifGT[{ ld[cnext ,c] }],
        ifLE[{ ld[ cInitial ,c] }]
    }]
};
"""


parse crn1