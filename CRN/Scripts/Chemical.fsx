// Patrikas Balsys 16-06-2022
// TODO everything here is deprecated, need to redo based on new understanding of the paper

#r "nuget: FParsec"

open FParsec

type Literal =
    | SpeciesLiteral of string
    | FloatLiteral of float

type Reaction = Reaction of Literal list * Literal list * float

// Reactants, Products, rate constant
type ModuleStmt =
    | ReactStmt of Reaction list
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
let loadConstructor (a, b) =
    ReactStmt [ Reaction([ a ], [ a; b ], 1)
                Reaction([ b ], [ FloatLiteral 0 ], 1) ]

let addConstructor (a, b, c) =
    ReactStmt [ Reaction([ a ], [ a; c ], 1)
                Reaction([ b ], [ b; c ], 1)
                Reaction([ c ], [ FloatLiteral 0 ], 1) ]

let subConstructor (a, b, c) = // TODO (what is H?)
    ReactStmt [ Reaction([ a ], [ a; c ], 1)
                Reaction([ b ], [ b; SpeciesLiteral "H" ], 1)
                Reaction([ c ], [ FloatLiteral 0 ], 1)
                Reaction([ c; SpeciesLiteral "H" ], [ FloatLiteral 0 ], 1) ]

let mulConstructor (a, b, c) =
    ReactStmt [ Reaction([ a; b ], [ a; b; c ], 1)
                Reaction([ c ], [ FloatLiteral 0 ], 1) ]

let divConstructor (a, b, c) =
    ReactStmt [ Reaction([ a ], [ a; c ], 1)
                Reaction([ b; c ], [ b ], 1) ]

let sqrtConstructor (a, b) =
    ReactStmt [ Reaction([ a ], [ a; b ], 1)
                Reaction([ b; b ], [ FloatLiteral 0 ], 0.5) ]

let cmpConstructor (a, b) = Compare(a, b)

let moduleStmt2SpeciesMaker id constructor =
    symbol $"{id}[" >>. speciesLiteral .>> symbol ","
    .>>. speciesLiteral
    .>> symbol "]"
    |>> constructor

let moduleStmt3SpeciesMaker id constructor =
    symbol $"{id}[" >>. speciesLiteral .>> symbol ","
    .>>. speciesLiteral
    .>> symbol ","
    .>>. speciesLiteral
    .>> symbol "]"
    |>> fun ((a, b), c) -> a, b, c
    |>> constructor

let load = moduleStmt2SpeciesMaker "ld" loadConstructor
let add = moduleStmt3SpeciesMaker "add" addConstructor
let sub = moduleStmt3SpeciesMaker "sub" subConstructor
let mul = moduleStmt3SpeciesMaker "mul" mulConstructor
let div = moduleStmt3SpeciesMaker "div" divConstructor
let sqrt = moduleStmt2SpeciesMaker "sqrt" sqrtConstructor
let cmp = moduleStmt2SpeciesMaker "cmp" cmpConstructor

let moduleStmt =
    choice [ load
             add
             sub
             mul
             div
             sqrt
             cmp ]
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
    |>> StepStmt

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

// counter.m
let crn1 =
    """
crn = {
conc[c,c0], conc[cInitial,c0],
conc[one,1], conc[zero,0],
step[{
    sub[c,one,cnext],
    cmp[c,zero]
}],
step[{
    ifGT[{ld[cnext,c]}],
    ifLE[{ld[cInitial,c]}]
}]
};
"""

// sequence.m
let crn2 =
    """
crn = {
conc[a, 3],
step[{
    rxn[a, b, 1]
}],
step[{
    rxn[b, c, 1]
}],
step[{
    rxn[c, a, 1]
}]
};
"""

// multiplication
let crn3 =
    """
crn = {
conc[a, 6],
conc[b, 2],
conc[c, 0],
step[{
    mul[a, b, c]
}],
};
"""

parse crn1