// Patrikas Balsys, 21-06-2022

module CRN.Simulation.ReactionSimulator

open CRN.Core.Types.Parser
open CRN.Core.Types.ReactionSimulator

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

// Converts parsed Crn code into a tuple of Concentration statements and Steps
let convertCRN crn =
    let sts = crn.Statements
    convertStatements ([], []) sts


//* ODE functions

// A modifiable state type used to keep track of various values during simulation
type ReactionState =
    { StepCounter: int
      Precision: float
      Reactions: Rxn list
      Compare: Option<Cmp>
      Derivatives: Map<string, float>
      Values: Map<string, float> }
    member this.S species = Map.find species this.Values
    member this.S' species = Map.find species this.Derivatives

// Counts occurences of x in xs
let rec count x xs =
    match xs with
    | [] -> 0
    | head :: tail when head = x -> 1 + count x tail
    | _ :: tail -> count x tail

// Net change of a species in a reaction
let netChange s reactants products = count s products - count s reactants

// Value of a species, current value is based on previous value and derivative
let speciesValue species (rs: ReactionState) =
    rs.S species + rs.Precision * (rs.S' species)

// Reactant product (used for the derivative)
let rProd reactants values =
    List.fold (fun s (r) -> s * ((Map.find r values))) 1.0 reactants

// Derivative value of a species, based on current values
let speciesDerivative species values reactions =
    List.fold
        (fun s (Rxn (r, p, k)) ->
            let temp = k * float (netChange (species) r p)

            if temp = 0.0 then
                s
            else
                s + temp * rProd r values)
        0.0
        reactions


//* Helper functions

// Gets the set of all species in Rxns
let speciesInReactions rxns =
    Set(List.fold (fun acc (Rxn (reacts, prods, _)) -> acc @ reacts @ prods) [] rxns)

// Gets the set of all species in Concentration statements
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

// For all species, calculate derivative values
let calculateDerivatives values reactions speciesList =
    Map(List.map (fun (s) -> (s, speciesDerivative s values reactions)) speciesList)

// For all species, calculate values
let calculateValues (rs: ReactionState) speciesList =
    Map(List.map (fun (s) -> (s, speciesValue s rs)) speciesList)

// Compares two values with approximate precision of 0.5
let compare a b =
    if abs (a - b) <= 0.5 then 0
    else if a > b then 1
    else -1

// Gets Rxns that will be used in step based on last Cmp statement
let getReactions (rs: ReactionState) (Step (d, GT, GE, EQ, LT, LE, _)) =
    match rs.Compare with
    | Some (Cmp (a, b)) ->
        match compare (rs.S a) (rs.S b) with
        | 1 -> d @ GT @ GE
        | -1 -> d @ LT @ LE
        | _ -> d @ GE @ EQ @ LE
    | None -> d

// Gets initial Rxns
let getInitReactions (Step (d, _, _, _, _, _, _)) = d

// Flattens all reactions in a step to a single list
let flattenStep (Step (d, GT, GE, EQ, LT, LE, _)) = d @ GT @ GE @ EQ @ LT @ LE

// Gets all species mentioned in the Crn
let getAllSpecies cons steps =
    Set.toList (
        Set.union
            (List.fold (fun all step -> Set.union all (speciesInReactions (flattenStep step))) Set.empty steps)
            (speciesInCons cons)
    )
// Gets the new Cmp if it exists, or uses previous one
let getCmp (Step (_, _, _, _, _, _, cmp)) prevCmp =
    match cmp with
    | None -> prevCmp
    | _ -> cmp

//* Reaction sequence
let reactionSeq prec stepTime args (cons, steps: list<Step>) =
    let stepCount = List.length steps
    let stepInterval = stepTime * int (1.0 / prec)
    let allSpecies = getAllSpecies cons steps
    let initValues = getInitValues allSpecies cons args
    let initReactions = getInitReactions (List.head steps)
    let initDerivatives = calculateDerivatives initValues initReactions allSpecies

    let initState =
        { StepCounter = 0
          Precision = prec
          Reactions = initReactions
          Compare = None
          Values = initValues
          Derivatives = initDerivatives }

    initState
    |> Seq.unfold (fun rs ->
        let isNextStep = rs.StepCounter % stepInterval = 0
        let nextStep = List.item (rs.StepCounter / stepInterval % stepCount) steps
        let values = calculateValues rs allSpecies

        let newState =
            if isNextStep then
                { rs with
                    StepCounter = rs.StepCounter + 1
                    Reactions = getReactions rs nextStep
                    Compare = getCmp nextStep rs.Compare
                    Derivatives = calculateDerivatives values rs.Reactions allSpecies
                    Values = values }
            else
                { rs with
                    StepCounter = rs.StepCounter + 1
                    Derivatives = calculateDerivatives values rs.Reactions allSpecies
                    Values = values }

        Some(rs.Values, newState))

// Generates reaction sequence from parsed CRN++ program
let simulate precision stepTime crn args =
    convertCRN crn |> reactionSeq precision stepTime args
