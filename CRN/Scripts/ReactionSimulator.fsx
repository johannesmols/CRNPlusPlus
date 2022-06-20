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
    | ConcentrationStmt (l1, l2) :: tail -> convertStatements (cons @ [ ConcentrationStmt(l1, l2) ], steps) tail
    | StepStmt (cmds) :: tail -> convertStatements (cons, steps @ [ convertCommands cmds ]) tail

let convertCRN crn =
    let sts = crn.Statements
    convertStatements ([], []) sts

// ODE functions
type ReactionState =
    { mutable StepCounter: int
      mutable Time: float
      mutable Precision: float
      mutable Reactions: ReactionStmt list
      mutable InitialValues: Map<string, float>
      mutable Values: Map<string, float>
      mutable NewValues: Map<string, float>
      mutable ValueFunctions: Map<string, float -> float>
      mutable DerivativeFunctions: Map<string, float -> float> }

    member this.S species =
        (this.ValueFunctions.TryFind species).Value

    member this.S' species =
        (this.DerivativeFunctions.TryFind species).Value

    member this.getVal species = (this.Values.TryFind species).Value

    member this.getInitVal species =
        (this.InitialValues.TryFind species).Value

    member this.updateValues = this.Values <- this.NewValues

let rec count x xs =
    match xs with
    | [] -> 0
    | head :: tail when head = x -> 1 + count x tail
    | head :: tail -> count x tail

let netChange s reactants products = count s products - count s reactants

let speciesValue species (rs: ReactionState) t =
    match t with
    | 0.0 -> rs.getInitVal species
    | _ ->
        rs.getVal species
        + rs.Precision * (rs.S' species) (t - rs.Precision)

let reactantProduct (reactants: list<Literal>) (rs: ReactionState) t =
    List.fold (fun s (SpeciesLiteral r) -> s * ((rs.getVal r))) 1.0 reactants

let speciesDerivative (species: string) (rs: ReactionState) t =
    List.fold
        (fun s (Reaction (r, p, FloatLiteral k)) ->
            let temp = (k * float (netChange (SpeciesLiteral species) r p))

            if temp = 0.0 then
                s
            else
                s + temp * reactantProduct r rs t)
        0.0
        rs.Reactions

// Helper functions
let speciesInStep step =
    Set(List.fold (fun acc (Reaction (reacts, prods, _)) -> acc @ reacts @ prods) [] step)

let speciesInCons cons =
    Set(List.map (fun (ConcentrationStmt (s, _)) -> s) cons)

let getInitValues cons =
    Map(List.map (fun (ConcentrationStmt (SpeciesLiteral s, FloatLiteral v)) -> (s, v)) cons)

let generateValueFunctions rs speciesList =
    Map(List.map (fun (SpeciesLiteral s) -> (s, speciesValue s rs)) speciesList)

let generateDerivativeFunctions rs speciesList =
    Map(List.map (fun (SpeciesLiteral s) -> (s, speciesDerivative s rs)) speciesList)

let generateValues (rs: ReactionState) speciesList =
    Map(List.map (fun (SpeciesLiteral s) -> (s, rs.S s rs.Time)) speciesList)

// Reaction sequence
let reactionSeq prec stepTime (cons, steps) =
    let stepCount = List.length steps
    let stepInterval = stepTime * int (1.0 / prec)
    let firstStep = List.head steps

    let reactionState =
        { StepCounter = 0
          Time = 0.0
          Precision = prec
          Reactions = []
          InitialValues = getInitValues cons
          Values = getInitValues cons
          NewValues = Map.empty
          ValueFunctions = Map.empty
          DerivativeFunctions = Map.empty }

    let allSpecies =
        Set.toList (Set.union (speciesInStep firstStep) (speciesInCons cons))

    reactionState
    |> Seq.unfold (fun rs ->
        if rs.StepCounter % stepInterval = 0 then
            rs.Reactions <- List.item (rs.StepCounter / stepInterval % stepCount) steps
            rs.ValueFunctions <- generateValueFunctions rs allSpecies
            rs.DerivativeFunctions <- generateDerivativeFunctions rs allSpecies


        rs.StepCounter <- rs.StepCounter + 1

        rs.NewValues <- generateValues rs allSpecies
        rs.updateValues
        rs.Time <- rs.Time + prec
        Some(rs.Values, rs))

let simulate prec stepTime crnpp =
    let result = parse crnpp

    match result with
    | Result.Ok crn -> convertCRN crn |> reactionSeq prec stepTime
    | Result.Error err -> failwith err

// --- Testing

// let crnpp1 = File.ReadAllText "./CRN/Scripts/examples/multiplication.crnpp"
// let crnpp2 = File.ReadAllText "./CRN/Scripts/examples/oscillator.crnpp"

// simulate 0.001 20 crnpp1
// |> Seq.take (80 * 1000)
// |> Seq.toList