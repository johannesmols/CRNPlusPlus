#load "./ReactionParser.fsx"

open ReactionParser
open System.IO


let convertModuleStmt =
    function
    | Load (a, b) ->
        [ Reaction([ a ], [ a; b ], FloatLiteral 1)
          Reaction([ b ], [], FloatLiteral 1) ]
    | Add (a, b, c) ->
        [ Reaction([ a ], [ a; c ], FloatLiteral 1)
          Reaction([ b ], [ b; c ], FloatLiteral 1)
          Reaction([ c ], [], FloatLiteral 1) ]
    | Subtract (a, b, c) ->
        [ Reaction([ a ], [ a; c ], FloatLiteral 1)
          Reaction([ b ], [ b; SpeciesLiteral("H") ], FloatLiteral 1)
          Reaction([ c ], [], FloatLiteral 1)
          Reaction([ c; SpeciesLiteral("H") ], [], FloatLiteral 1) ]
    | Multiply (a, b, c) ->
        [ Reaction([ a; b ], [ a; b; c ], FloatLiteral 1)
          Reaction([ c ], [], FloatLiteral 1) ]
    | Divide (a, b, c) ->
        [ Reaction([ a ], [ a; c ], FloatLiteral 1)
          Reaction([ b; c ], [ b ], FloatLiteral 1) ]
    | SquareRoot (a, b) ->
        [ Reaction([ a ], [ a; b ], FloatLiteral 1)
          Reaction([ b; b ], [], FloatLiteral 0.5) ]
    | Compare (a, b) -> [ Reaction([], [], FloatLiteral 1) ]

let rec convertCommands =
    function
    | [] -> []
    | ConditionalStmt (_) :: tail -> convertCommands tail
    | ModuleStmt (ms) :: tail -> convertModuleStmt ms @ convertCommands tail
    | ReactionStmt (rs) :: tail -> [ rs ] @ convertCommands tail

let rec convertStatements (cons, steps) =
    function
    | [] -> (cons, steps)
    | ConcentrationStmt (l1, l2) :: tail -> convertStatements (ConcentrationStmt(l1, l2) :: cons, steps) tail
    | StepStmt (cmds) :: tail -> convertStatements (cons, convertCommands cmds :: steps) tail

let convertCRN crn =
    let sts = crn.Statements
    convertStatements ([], []) sts

// TODO simulation
let simulate crn =
    let rn = convertCRN crn
    rn

//* --- Testing

let trySimulate crnpp =
    let result = parse crnpp

    match result with
    | Result.Ok crn -> simulate crn
    | Result.Error err -> failwith err

let crnpp1 = File.ReadAllText "./CRN/Scripts/examples/multiplication.crnpp"

trySimulate crnpp1