// Johannes Mols, 15-06-2022

module CRN.Simulation.DiscreteSimulator

open CRN.Core.Types.Parser
open CRN.Core.Types.Simulator

module Seq =
  /// Infinitely repeat a sequence of values
  let repeat items = 
    seq { while true do yield! items }

/// Replace concentration statements where value is another species with actual values from the arguments
let replaceConcentrationsWithArguments crn (args: Map<string, float>) =
    { crn with Arguments = []; Statements = crn.Statements |> List.map (fun s ->
            match s with
            | StepStmt _ -> s
            | ConcentrationStmt(target, value) ->
                match value with
                | FloatLiteral _ -> s
                | SpeciesLiteral s -> ConcentrationStmt(target, args[s] |> FloatLiteral)) }
    
// Construct an initial state from the concentration statements in a program
let constructInitialState crn =
    { Comparison = 0., 0.; Concentrations = crn.Statements |> List.choose (fun s ->
        match s with
        | StepStmt _ -> None
        | ConcentrationStmt(s, l) ->
            match s, l with
            | SpeciesLiteral s, FloatLiteral f -> Some(s, f)
            | _ -> failwith "Can't construct initial state with concentrations where value is not a float.") |> Map.ofList }
    
/// Simulate a step of a program on a given state
let simulateStep (state: State) (step: Statement) =
    let rec addZeroValuesIfNotExisting species state =
        match species with
        | [] -> state
        | h::t ->
            if not(state.Concentrations.ContainsKey h) then
                { state with Concentrations = state.Concentrations.Add(h, 0.) }
                |> addZeroValuesIfNotExisting t
            else
                state |> addZeroValuesIfNotExisting t
    
    let performOperator2 state i t op_l op =
        match i, t with
        | SpeciesLiteral i, SpeciesLiteral t ->
            let state = addZeroValuesIfNotExisting [i] state
            { state with Concentrations = state.Concentrations.Add(t, op state.Concentrations[i]) }
        | _ -> failwith $"Attempted to use {op_l} module with float literals."
    
    let performOperator3 state i1 i2 t op_l op =
        match i1, i2, t with
        | SpeciesLiteral i1, SpeciesLiteral i2, SpeciesLiteral t ->
            let state = addZeroValuesIfNotExisting [i1; i2] state
            { state with Concentrations = state.Concentrations.Add(t, op state.Concentrations[i1] state.Concentrations[i2]) }
        | _ -> failwith $"Attempted to use {op_l} module with float literals."
    
    let moduleStmt (state: State) = function
        | Load(i, target) -> performOperator2 state i target "ld" id
        | Add(i1, i2, target) -> performOperator3 state i1 i2 target "add" (+)
        | Subtract(i1, i2, target) -> performOperator3 state i1 i2 target "sub" (fun a b -> if a > b then a - b else 0.)
        | Multiply(i1, i2, target) -> performOperator3 state i1 i2 target "mul" (*)
        | Divide(i1, i2, target) -> performOperator3 state i1 i2 target "div" (/)
        | SquareRoot(i, target) -> performOperator2 state i target "sqrt" sqrt
        | Compare(i, target) ->
            match i, target with
            | SpeciesLiteral i, SpeciesLiteral t ->
                { state with Comparison = state.Concentrations[i], state.Concentrations[t] }
            | _ -> failwith "Attempted to use comparison module with float literals."
            
    let rec conditionalStmt (state: State) = function
        | IfGreaterThan cmds -> if state.IsGreater then simulate state cmds else state
        | IfGreaterThanOrEquals cmds -> if state.IsGreaterOrEquals then simulate state cmds else state
        | IfEquals cmds -> if state.IsEqual then simulate state cmds else state
        | IfLesserThan cmds -> if state.IsLesser then simulate state cmds else state
        | IfLesserThanOrEquals cmds -> if state.IsLesserOrEquals then simulate state cmds else state
    
    and simulate (state: State) (steps: Command list) =
        match steps with
        | [] -> state
        | cmd :: rem ->
            let newState =
                match cmd with
                | ModuleStmt m -> moduleStmt state m
                | ConditionalStmt c -> conditionalStmt state c
                | ReactionStmt _ -> failwith "This interpreter does not support reaction statements. Please use the reaction simulator instead."
            simulate newState rem
    
    match step with
    | ConcentrationStmt _ -> failwith "The statement must be a step statement."
    | StepStmt cmds -> simulate state cmds

let simulate crn (args: Map<string, float>) =
    if crn.Arguments.Length <> args.Count then
        failwith $"Expected {crn.Arguments.Length} arguments (%A{crn.Arguments}), but received {args.Count}."
        
    let crn = replaceConcentrationsWithArguments crn args
    let initialState = constructInitialState crn
    
    let steps = crn.Statements
                |> List.filter (fun s -> match s with | StepStmt _ -> true | _ -> false)
                |> Seq.repeat
    
    (initialState, steps) ||> Seq.scan simulateStep |> Seq.cache