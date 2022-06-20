// Johannes Mols, 15-06-2022

module CRN.Core.CRNPP.SemanticAnalyzer

open CRN.Core.CRNPP.Types

/// Get a list of species names where the value has not been defined in the program
let extractMissingArguments crn =
    crn.Statements
    |> List.choose (fun s ->
        match s with
        | ConcentrationStmt(_, value) ->
            match value with
            | SpeciesLiteral s -> Some s
            | _ -> None
        | _ -> None )
    |> List.distinct

/// Check that concentration statements appear before any step statements
let concentrationsAreDefinedBeforeSteps crn =
    let conc, steps = crn.Statements |> List.mapi (fun i s ->
                            match s with
                            | ConcentrationStmt _ -> true, i
                            | StepStmt _ -> false, i)
                        |> List.partition fst
                        |> fun (a, b) -> a |> List.map snd, b |> List.map snd
    conc |> List.forall (fun ci -> steps |> List.forall (fun si -> ci < si)) 
    
/// Check that all module statements adhere to the restrictions of Table 1 in Vasic et al.
let moduleOutputSpeciesAreDifferentFromInput crn =
    let stmtSatisfiesRules (s: Statement) =
        let rec cmdSatisfiesRules (c: Command) =
            match c with
            | ModuleStmt moduleStmt ->
                match moduleStmt with
                | Load(i, o) | SquareRoot(i, o) | Compare(i, o) -> i <> o
                | Add(i1, i2, o) | Subtract(i1, i2, o) | Multiply(i1, i2, o) | Divide(i1, i2, o) -> i1 <> o && i2 <> o
            | ConditionalStmt conditionalStmt ->
                match conditionalStmt with
                | IfGreaterThan c | IfGreaterThanOrEquals c | IfEquals c | IfLesserThan c | IfLesserThanOrEquals c ->
                    c |> List.forall cmdSatisfiesRules
        
        match s with
        | ConcentrationStmt _ -> true
        | StepStmt commands -> commands |> List.forall cmdSatisfiesRules 
    
    crn.Statements
    |> List.forall stmtSatisfiesRules
    
let statementsAreNotConflicting crn =
    true // TODO

/// Analyze a CRN for semantic errors and return either an error or the program if everything is fine
let analyze (crn: Crn) : Result<Crn, string> =
    let crn = { crn with Arguments = extractMissingArguments crn }
    
    if not(concentrationsAreDefinedBeforeSteps crn) then
        Result.Error "All concentration declarations must happen before any steps."
    else if not(moduleOutputSpeciesAreDifferentFromInput crn) then
        Result.Error "Module statements must not have an output that is the same as an input."
    else if not(statementsAreNotConflicting crn) then
        Result.Error "Statements within a step module are conflicting."
    else
        Result.Ok crn