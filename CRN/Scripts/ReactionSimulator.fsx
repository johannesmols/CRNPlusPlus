#load "./ReactionParser.fsx"

open ReactionParser
open System.IO

type Rxn = Rxn of string list * string list * float
type Cmp = Cmp of string * string
type Step = Step of Rxn list * Rxn list * Rxn list * Rxn list * Rxn list * Rxn list * Option<Cmp>

// Conversion functions
let convertModuleStmt =
    function
    | Load (SpeciesLiteral a, SpeciesLiteral b) ->
        [ Rxn([ a ], [ a; b ], 1)
          Rxn([ b ], [], 1) ]
    | Add (SpeciesLiteral a, SpeciesLiteral b, SpeciesLiteral c) ->
        [ Rxn([ a ], [ a; c ], 1)
          Rxn([ b ], [ b; c ], 1)
          Rxn([ c ], [], 1) ]
    | Subtract (SpeciesLiteral a, SpeciesLiteral b, SpeciesLiteral c) ->
        [ Rxn([ a ], [ a; c ], 1)
          Rxn([ b ], [ b; c + "H" ], 1)
          Rxn([ c ], [], 1)
          Rxn([ c; c + "H" ], [], 1) ]
    | Multiply (SpeciesLiteral a, SpeciesLiteral b, SpeciesLiteral c) ->
        [ Rxn([ a; b ], [ a; b; c ], 1)
          Rxn([ c ], [], 1) ]
    | Divide (SpeciesLiteral a, SpeciesLiteral b, SpeciesLiteral c) ->
        [ Rxn([ a ], [ a; c ], 1)
          Rxn([ b; c ], [ b ], 1) ]
    | SquareRoot (SpeciesLiteral a, SpeciesLiteral b) ->
        [ Rxn([ a ], [ a; b ], 1)
          Rxn([ b; b ], [], 0.5) ]
    | _ -> []

let convertReactionStmt =
    function
    | Reaction (rs, ps, FloatLiteral k) ->
        [ Rxn(
              List.map
                  (function
                  | (SpeciesLiteral r) -> r
                  | _ -> failwith "invalid reaction statement")
                  rs,
              List.map
                  (function
                  | (SpeciesLiteral p) -> p
                  | _ -> failwith "invalid reaction statement")
                  ps,
              k
          ) ]
    | _ -> []

let rec convertCommands' =
    function
    | [] -> []
    | ConditionalStmt _ :: tail -> convertCommands' tail
    | ModuleStmt (Compare (SpeciesLiteral a, SpeciesLiteral b)) :: tail -> convertCommands' tail
    | ModuleStmt (ms) :: tail -> convertModuleStmt ms @ convertCommands' tail
    | ReactionStmt (rs) :: tail -> convertReactionStmt rs @ convertCommands' tail

let rec convertCommands (Step (d, GT, GE, EQ, LT, LE, cmp)) =
    function
    | [] -> Step(d, GT, GE, EQ, LT, LE, cmp)
    | ConditionalStmt (IfGreaterThan cmds) :: tail ->
        convertCommands (Step(d, convertCommands' cmds @ GT, GE, EQ, LT, LE, cmp)) tail
    | ConditionalStmt (IfGreaterThanOrEquals cmds) :: tail ->
        convertCommands (Step(d, GT, convertCommands' cmds @ GE, EQ, LT, LE, cmp)) tail
    | ConditionalStmt (IfEquals cmds) :: tail ->
        convertCommands (Step(d, GT, GE, convertCommands' cmds @ EQ, LT, LE, cmp)) tail
    | ConditionalStmt (IfLesserThan cmds) :: tail ->
        convertCommands (Step(d, GT, GE, EQ, convertCommands' cmds @ LT, LE, cmp)) tail
    | ConditionalStmt (IfLesserThanOrEquals cmds) :: tail ->
        convertCommands (Step(d, GT, GE, EQ, LT, convertCommands' cmds @ LE, cmp)) tail
    | ModuleStmt (Compare (SpeciesLiteral a, SpeciesLiteral b)) :: tail ->
        convertCommands (Step(d, GT, GE, EQ, LT, LE, Some(Cmp(a, b)))) tail
    | ModuleStmt (ms) :: tail -> convertCommands (Step(convertModuleStmt ms @ d, GT, GE, EQ, LT, LE, cmp)) tail
    | ReactionStmt (rs) :: tail -> convertCommands (Step(convertReactionStmt rs @ d, GT, GE, EQ, LT, LE, cmp)) tail

let rec convertStatements (cons, steps) =
    function
    | [] -> (cons, steps)
    | ConcentrationStmt (l1, l2) :: tail -> convertStatements (cons @ [ ConcentrationStmt(l1, l2) ], steps) tail
    | StepStmt (cmds) :: tail ->
        convertStatements
            (cons,
             steps
             @ [ convertCommands (Step([], [], [], [], [], [], None)) cmds ])
            tail

let convertCRN crn =
    let sts = crn.Statements
    convertStatements ([], []) sts

// ODE functions
type ReactionState =
    { mutable StepCounter: int
      mutable Time: float
      mutable Precision: float
      mutable Reactions: Rxn list
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

let reactantProduct reactants (rs: ReactionState) t =
    List.fold (fun s (r) -> s * ((rs.getVal r))) 1.0 reactants

let speciesDerivative (species: string) (rs: ReactionState) t =
    List.fold
        (fun s (Rxn (r, p, k)) ->
            let temp = (k * float (netChange (species) r p))

            if temp = 0.0 then
                s
            else
                s + temp * reactantProduct r rs t)
        0.0
        rs.Reactions

// Helper functions
let speciesInReactions step =
    Set(List.fold (fun acc (Rxn (reacts, prods, _)) -> acc @ reacts @ prods) [] step)

let speciesInCons cons =
    Set(
        List.map
            (function
            | (ConcentrationStmt (SpeciesLiteral s, _)) -> s
            | _ -> failwith "invalid concentration statement inside cons")
            cons
    )

let getInitValues species cons args =
    Map(
        List.map (fun (s) -> (s, 0.0)) species
        @ List.map
            (function
            | (ConcentrationStmt (SpeciesLiteral s, FloatLiteral v)) -> (s, v)
            | (ConcentrationStmt (SpeciesLiteral s, SpeciesLiteral v)) -> (s, Map.find v args)
            | _ -> failwith "invalid concentration statement inside cons")
            cons
    )

let generateValueFunctions rs speciesList =
    Map(List.map (fun (s) -> (s, speciesValue s rs)) speciesList)

let generateDerivativeFunctions rs speciesList =
    Map(List.map (fun (s) -> (s, speciesDerivative s rs)) speciesList)

let generateValues (rs: ReactionState) speciesList =
    Map(List.map (fun (s) -> (s, rs.S s rs.Time)) speciesList)

let compare a b =
    if abs (a - b) < 0.5 then 0
    else if a > b then 1
    else -1

let getReactions (rs: ReactionState) (Step (d, GT, GE, EQ, LT, LE, _)) prevCmp =
    match prevCmp with
    | Some (Cmp (a, b)) ->
        match compare (rs.getVal a) (rs.getVal b) with
        | 1 -> d @ GT @ GE
        | -1 -> d @ LT @ LE
        | _ -> d @ GE @ EQ @ LE
    | None -> d

let flattenStep (Step (d, GT, GE, EQ, LT, LE, _)) = d @ GT @ GE @ EQ @ LT @ LE

let getCmp (Step (_, _, _, _, _, _, cmp)) = cmp

// Reaction sequence
let reactionSeq prec stepTime (cons, steps: list<Step>) args =
    let stepCount = List.length steps
    let stepInterval = stepTime * int (1.0 / prec)



    let allSpecies =
        Set.toList (
            Set.union
                (List.fold (fun all step -> Set.union all (speciesInReactions (flattenStep step))) Set.empty steps)
                (speciesInCons cons)
        )

    let reactionState =
        { StepCounter = 0
          Time = 0.0
          Precision = prec
          Reactions = []
          InitialValues = getInitValues allSpecies cons args // TODO remove, use just Values instead
          Values = getInitValues allSpecies cons args
          NewValues = Map.empty
          ValueFunctions = Map.empty
          DerivativeFunctions = Map.empty }

    reactionState
    |> Seq.unfold (fun rs ->
        if rs.StepCounter % stepInterval = 0 then
            let stepIndex = (rs.StepCounter / stepInterval) % stepCount
            let step = List.item stepIndex steps

            let prevCmp =
                if stepIndex = 0 then
                    None
                else
                    List.item (stepIndex - 1) steps |> getCmp

            rs.Reactions <- getReactions rs step prevCmp
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

// let crn1 = File.ReadAllText "./CRN/Scripts/examples/basic/mul.crnpp"
// let crn2 = File.ReadAllText "./CRN/Scripts/examples/basic/ld.crnpp"
let crn3 = File.ReadAllText "./CRN/Scripts/examples/counter.crnpp"

simulate 0.001 20 crn3 (Map [ ("a0", 3.0) ])
|> Seq.take (200 * 1000)
|> Seq.toList