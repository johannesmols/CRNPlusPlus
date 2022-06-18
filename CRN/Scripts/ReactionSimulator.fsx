#load "./ReactionParser.fsx"

open ReactionParser
open System.IO

// Conversion functions
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

// ODE functions

let rec count x xs =
    match xs with
    | [] -> 0
    | head :: tail when head = x -> 1 + count x tail
    | head :: tail -> count x tail

let netChange s rs ps = count s ps - count s rs

type ReactionState =
    { Values: Map<string, float>
      ValueFunctions: Map<string, string -> float -> ReactionState -> float -> float -> float -> float>
      DerivativeFunctions: Map<string, float -> float> }

    member this.S species =
        (this.ValueFunctions.TryFind species).Value

    member this.S' species =
        (this.DerivativeFunctions.TryFind species).Value

let rs1 =
    { Values = Map.empty
      ValueFunctions = Map.empty
      DerivativeFunctions = Map.empty }

let rec speciesValue species prec (rs: ReactionState) startVal prevVal t =
    match t with
    | 0.0 -> startVal
    | _ -> prevVal + prec * (rs.S' species) (t - prec)


let reactantProduct (reactants: list<string>) prec (rs: ReactionState) startVal prevVal t =
    List.fold (fun s r -> s * ((rs.S r) r prec rs startVal prevVal t)) 1.0 reactants

let rec speciesDerivative species rxns =
    List.fold (fun s (Reaction (rs, ps, FloatLiteral k)) -> s + (k * float (netChange species rs ps))) 0.0 rxns


// TODO simulation
let simulate crn =
    let rn = convertCRN crn
    // rn |> constructState |> constructSequence
    rn

let trySimulate crnpp =
    let result = parse crnpp

    match result with
    | Result.Ok crn -> simulate crn
    | Result.Error err -> failwith err



//* --- Testing

let crnpp1 = File.ReadAllText "./CRN/Scripts/examples/multiplication.crnpp"

trySimulate crnpp1