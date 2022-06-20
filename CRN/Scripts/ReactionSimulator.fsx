#load "./ReactionParser.fsx"

open ReactionParser
open System.IO


//* New type declarations interpretation

// Reaction: reactants, products, reaction rate
type Rxn = Rxn of string list * string list * float
// Comparison statement: species, species
type Cmp = Cmp of string * string
// Step: default Rxns, IfGT Rxns, IfGE Rxns, IfEQ Rxns, IfLT Rxns, IfLE Rxns, Cmp
type Step = Step of Rxn list * Rxn list * Rxn list * Rxn list * Rxn list * Rxn list * Option<Cmp>


//* Conversion statements

// Converts a module statement to a list of Rxns
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

// Converts a Reaction statement to a single Rxn encased in a list
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

// Converts a list of commands to a list of Rxns
let rec convertCommands' =
    function
    | [] -> []
    | ConditionalStmt _ :: tail -> convertCommands' tail
    | ModuleStmt (Compare (SpeciesLiteral a, SpeciesLiteral b)) :: tail -> convertCommands' tail
    | ModuleStmt (ms) :: tail -> convertModuleStmt ms @ convertCommands' tail
    | ReactionStmt (rs) :: tail -> convertReactionStmt rs @ convertCommands' tail

// Converts a list of commands into a Step via accumulation
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

// Converts a list of Statements into a tuple of Concentration statements and Steps
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

// Converts Crn into a tuple of Concentration statements and Steps
let convertCRN crn =
    let sts = crn.Statements
    convertStatements ([], []) sts


//* ODE functions

// A modifiable state type used to keep track of various values during simulation
type ReactionState =
    { mutable StepCounter: int
      mutable Time: float
      mutable Precision: float
      mutable Reactions: Rxn list
      mutable Compare: Option<Cmp>
      mutable Values: Map<string, float>
      mutable NewValues: Map<string, float>
      mutable ValueFunctions: Map<string, float -> float>
      mutable DerivativeFunctions: Map<string, float -> float> }

    member this.S species =
        (this.ValueFunctions.TryFind species).Value

    member this.S' species =
        (this.DerivativeFunctions.TryFind species).Value

    member this.getVal species = (this.Values.TryFind species).Value

    member this.updateValues = this.Values <- this.NewValues

// Counts occurences of x in xs
let rec count x xs =
    match xs with
    | [] -> 0
    | head :: tail when head = x -> 1 + count x tail
    | _ :: tail -> count x tail

// Net change of a species in a reaction
let netChange s reactants products = count s products - count s reactants

// Value function of a species, current value is based on previous value and derivative
let speciesValue species (rs: ReactionState) t =
    match t with
    | 0.0 -> rs.getVal species
    | _ ->
        rs.getVal species
        + rs.Precision * (rs.S' species) (t - rs.Precision)

// Reactant product (used for the derivative function)
let rProd reactants (rs: ReactionState) t =
    List.fold (fun s (r) -> s * ((rs.getVal r))) 1.0 reactants

// Derivative function of a species
let speciesDerivative (species: string) (rs: ReactionState) t =
    List.fold
        (fun s (Rxn (r, p, k)) ->
            let temp = k * float (netChange (species) r p)

            if temp = 0.0 then
                s
            else
                s + t * rProd r rs t)
        0.0
        rs.Reactions


//* Helper functions

// Gets a set of all species in a step
let speciesInReactions step =
    Set(List.fold (fun acc (Rxn (reacts, prods, _)) -> acc @ reacts @ prods) [] step)

// Gets a set of all species in Concentration statements
let speciesInCons cons =
    Set(
        List.map
            (function
            | (ConcentrationStmt (SpeciesLiteral s, _)) -> s
            | _ -> failwith "invalid concentration statement inside cons")
            cons
    )

// Gets initial values for species: float if specified as a number, and from args if not
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

// For all species, generates their value functions
let generateValueFunctions rs speciesList =
    Map(List.map (fun (s) -> (s, speciesValue s rs)) speciesList)

// For all species, generates their derivative functions
let generateDerivativeFunctions rs speciesList =
    Map(List.map (fun (s) -> (s, speciesDerivative s rs)) speciesList)

// For all species, calculate current values
let calculateValues (rs: ReactionState) speciesList =
    Map(List.map (fun (s) -> (s, rs.S s rs.Time)) speciesList)

// Compares two values with approximate precision of 0.5
let compare a b =
    if abs (a - b) < 0.5 then 0
    else if a > b then 1
    else -1

// Gets Rxns that will be used in step based on last Cmp statement
let getReactions (rs: ReactionState) (Step (d, GT, GE, EQ, LT, LE, _)) =
    match rs.Compare with
    | Some (Cmp (a, b)) ->
        match compare (rs.getVal a) (rs.getVal b) with
        | 1 -> d @ GT @ GE
        | -1 -> d @ LT @ LE
        | _ -> d @ GE @ EQ @ LE
    | None -> d

// Flattens all reactions in a step to a single list
let flattenStep (Step (d, GT, GE, EQ, LT, LE, _)) = d @ GT @ GE @ EQ @ LT @ LE

// Gets the new Cmp if it exists, or uses previous one
let getCmp (Step (_, _, _, _, _, _, cmp)) prevCmp =
    match cmp with
    | None -> prevCmp
    | _ -> cmp

//* Reaction sequence
let reactionSeq prec stepTime args (cons, steps: list<Step>) =
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
          Compare = None
          Values = getInitValues allSpecies cons args
          NewValues = Map.empty
          ValueFunctions = Map.empty
          DerivativeFunctions = Map.empty }

    reactionState
    |> Seq.unfold (fun rs ->
        if rs.StepCounter % stepInterval = 0 then
            let step = List.item (rs.StepCounter / stepInterval % stepCount) steps
            rs.Compare <- getCmp step rs.Compare
            rs.Reactions <- getReactions rs step
            rs.ValueFunctions <- generateValueFunctions rs allSpecies
            rs.DerivativeFunctions <- generateDerivativeFunctions rs allSpecies

        rs.StepCounter <- rs.StepCounter + 1
        rs.NewValues <- calculateValues rs allSpecies
        rs.updateValues
        rs.Time <- rs.Time + prec
        Some(rs.Values, rs))

// Generates reaction sequence from crnpp code
let simulate prec stepTime crnpp args =
    let result = parse crnpp

    match result with
    | Result.Ok crn -> convertCRN crn |> reactionSeq prec stepTime args
    | Result.Error err -> failwith err

// --- Testing

// let crnCou = File.ReadAllText "./CRN/Scripts/examples/counter.crnpp"
// let crnDiv = File.ReadAllText "./CRN/Scripts/examples/division.crnpp"
// let crnEul = File.ReadAllText "./CRN/Scripts/examples/euler.crnpp"
// let crnFac = File.ReadAllText "./CRN/Scripts/examples/factorial.crnpp"
// let crnGcd = File.ReadAllText "./CRN/Scripts/examples/gcd.crnpp"
// let crnOsc = File.ReadAllText "./CRN/Scripts/examples/oscillator.crnpp"
// let crnPi = File.ReadAllText "./CRN/Scripts/examples/pi.crnpp"
// let crnSeq = File.ReadAllText "./CRN/Scripts/examples/sequence.crnpp"
// let crnSqu = File.ReadAllText "./CRN/Scripts/examples/squareroot.crnpp"
// let crnSub = File.ReadAllText "./CRN/Scripts/examples/subtract.crnpp"


// simulate 0.001 20 crn3 (Map [ ("a0", 3.0) ])
// |> Seq.take (200 * 1000)
// |> Seq.toList