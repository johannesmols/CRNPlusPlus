module CRN.Core.CRNPP.Interpreter

open System.Runtime.Intrinsics.X86
open CRN.Core.CRNPP.Types

type State = {
    Concentrations : Map<string, float>
    Comparison : float * float
}

/// Replace concentration statements where value is another species with actual values from the arguments
let replaceConcentrationsWithArguments crn (args: Map<string, float>) =
    { crn with Statements = crn.Statements
            |> List.map (fun s ->
                match s with
                | StepStmt _ -> s
                | ConcentrationStmt (species, tmp) ->
                    ConcentrationStmt(species, args[match tmp with
                                                    | SpeciesLiteral s -> s
                                                    | _ -> failwith $"Species name was not of type SpeciesLiteral ({species})."]
                                               |> FloatLiteral)); Arguments = [] }
    
let simulateStep (state: State) (step: Statement) =
    let moduleStmt (state: State) = function
        | Load(i, target) ->
            match i, target with
            | SpeciesLiteral from, SpeciesLiteral target ->
                { state with Concentrations = state.Concentrations.Add(target, state.Concentrations[from]) }
            | _ -> failwith "Attempted to use load module with float literals."
        | Add(i1, i2, target) ->
            match i1, i2, target with
            | SpeciesLiteral i1, SpeciesLiteral i2, SpeciesLiteral t ->
                { state with Concentrations = state.Concentrations.Add(t, state.Concentrations[i1] + state.Concentrations[i2]) }
            | _ -> failwith "Attempted to use add module with float literals."
        | Subtract(i1, i2, target) ->
            match i1, i2, target with
            | SpeciesLiteral i1, SpeciesLiteral i2, SpeciesLiteral t ->
                { state with Concentrations =
                                let a = state.Concentrations[i1]
                                let b = state.Concentrations[i2]
                                match a > b with
                                | true -> state.Concentrations.Add(t, a - b)
                                | false -> state.Concentrations.Add(t, 0) }
            | _ -> failwith "Attempted to use subtract module with float literals."
        | Multiply(i1, i2, target) -> state
        | Divide(i1, i2, target) -> state
        | SquareRoot(i, target) -> state
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
    
    true