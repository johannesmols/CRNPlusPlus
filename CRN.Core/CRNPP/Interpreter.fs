module CRN.Core.CRNPP.Interpreter

open CRN.Core.CRNPP.Types

module Seq =
  /// Infinitely repeat a sequence of values
  let repeat items = 
    seq { while true do yield! items }

type State = {
    Concentrations : Map<string, float>
    Comparison : float * float
}

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
    
let simulateStep (state: State) (step: Statement) =
    let performOperator2 state i t op_l op =
        match i, t with
        | SpeciesLiteral i, SpeciesLiteral t -> 
            { state with Concentrations = state.Concentrations.Add(t, op state.Concentrations[i]) }
        | _ -> failwith $"Attempted to use {op_l} operator with float literals."
    
    let performOperator3 state i1 i2 t op_l op =
        match i1, i2, t with
        | SpeciesLiteral i1, SpeciesLiteral i2, SpeciesLiteral t -> 
            { state with Concentrations = state.Concentrations.Add(t, op state.Concentrations[i1] state.Concentrations[i2]) }
        | _ -> failwith $"Attempted to use {op_l} operator with float literals."
    
    let moduleStmt (state: State) = function
        | Load(i, target) -> performOperator2 state i target "ld" id
        | Add(i1, i2, target) -> performOperator3 state i1 i2 target "add" (+)
        | Subtract(i1, i2, target) -> performOperator3 state i1 i2 target "sub" (fun a b -> match a > b with | true -> a - b | false -> 0.)
        | Multiply(i1, i2, target) -> performOperator3 state i1 i2 target "mul" (*)
        | Divide(i1, i2, target) -> performOperator3 state i1 i2 target "div" (/)
        | SquareRoot(i, target) -> performOperator2 state i target "sqrt" sqrt
        | Compare(i, target) -> state
    
    let conditionalStmt (state: State) = function
        | IfGreaterThan cmds -> state
        | IfGreaterThanOrEquals cmds -> state
        | IfEquals cmds -> state
        | IfLesserThan cmds -> state
        | IfLesserThanOrEquals cmds -> state
    
    let rec simulate (state: State) (steps: Command list) =
        match steps with
        | [] -> state
        | cmd :: rem ->
            let newState =
                match cmd with
                | ModuleStmt m -> moduleStmt state m
                | ConditionalStmt c -> conditionalStmt state c
            simulate newState rem
    
    match step with
    | ConcentrationStmt _ -> failwith "The statement must be a step statement."
    | StepStmt cmds -> simulate state cmds

let interpret crn (args: Map<string, float>) =
    if crn.Arguments.Length <> args.Count then
        failwith $"Expected {crn.Arguments.Length} arguments, but received {args.Count}."
        
    let crn = replaceConcentrationsWithArguments crn args
    let state = constructInitialState crn
    
    let steps = crn.Statements
                |> List.filter (fun s -> match s with | StepStmt _ -> true | _ -> false)
                |> Seq.repeat
    
    let states = seq {
        yield! (state, steps) ||> Seq.scan simulateStep
    }
    
    let first10 = states |> Seq.take 10
    
    true