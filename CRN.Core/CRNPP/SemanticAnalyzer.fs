// Johannes Mols, 15-06-2022

module CRN.Core.CRNPP.SemanticAnalyzer

open CRN.Core.CRNPP.Types

/// Extract only the step statements from a program
let getStepStatements crn =
    crn.Statements |> List.choose (fun s ->
        match s with
        | ConcentrationStmt _ -> None
        | StepStmt cmds -> Some cmds)

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
            | ReactionStmt _ -> true // Ignore reaction statements
        
        match s with
        | ConcentrationStmt _ -> true
        | StepStmt commands -> commands |> List.forall cmdSatisfiesRules 
    
    crn.Statements
    |> List.forall stmtSatisfiesRules
    
/// Check that statements within single steps are not conflicting. Conflicting statements are:
/// 1. Comparison and conditional execution cannot happen in the same step, as comparison needs to be done beforehand
/// 2. Reading and writing to the same species
let statementsAreNotConflicting crn =
    let comparisonAndConditionalExecutionAreSeparated crn =    
        let rec cmdContainsComparison = function
            | ModuleStmt m ->
                match m with
                | Compare _ -> true
                | _ -> false
            | ConditionalStmt c ->
                match c with
                | IfGreaterThan c | IfGreaterThanOrEquals c | IfEquals c | IfLesserThan c | IfLesserThanOrEquals c ->
                    c |> List.forall cmdContainsComparison
            | ReactionStmt _ -> false // Ignore reaction statements
        
        getStepStatements crn
        |> List.forall (fun cmds ->
            let hasComparison = cmds |> List.exists cmdContainsComparison
            let hasConditional = cmds |> List.exists (fun c -> match c with | ConditionalStmt _ -> true | _ -> false)
            not(hasComparison && hasConditional))
        
    let noReadingAndWritingToSameSpecies crn =
        let rec constructReadWriteTable (read: string list) (write: string list) = function
            | [] -> read, write
            | cmd::rest ->
                match cmd with
                | ConditionalStmt stmt ->
                    match stmt with
                    | IfGreaterThan c | IfGreaterThanOrEquals c | IfEquals c | IfLesserThan c | IfLesserThanOrEquals c ->
                        constructReadWriteTable read write c
                        |> fun (r, w) -> constructReadWriteTable r w rest
                | ModuleStmt stmt ->
                    match stmt with
                    | Load(SpeciesLiteral i, SpeciesLiteral t)
                    | SquareRoot(SpeciesLiteral i, SpeciesLiteral t) -> i::read, t::write
                    | Add(SpeciesLiteral i1, SpeciesLiteral i2, SpeciesLiteral t)
                    | Subtract(SpeciesLiteral i1, SpeciesLiteral i2, SpeciesLiteral t)
                    | Multiply(SpeciesLiteral i1, SpeciesLiteral i2, SpeciesLiteral t)
                    | Divide(SpeciesLiteral i1, SpeciesLiteral i2, SpeciesLiteral t) -> i1::i2::read, t::write
                    | Compare(SpeciesLiteral i, SpeciesLiteral t) -> i::t::read, write
                    | _ -> failwith "Attempted to use float literals in module statements."
                    |> fun (r, w) -> constructReadWriteTable r w rest
                | ReactionStmt _ -> constructReadWriteTable read write rest // Ignore reactions statements
        
        getStepStatements crn
        |> List.map (fun cmds -> cmds |> constructReadWriteTable [] [])
        |> List.forall (fun (r, w) ->
            w |> List.distinct |> List.length = w.Length && // Not writing to same variables multiple times
            // TODO: Does not work because conditional statements are flattened. Two conditionals might write to the same species, but only one is executed.
            (Set.ofList r, Set.ofList w) ||> Set.intersect = Set.empty) // Read and write actions don't use same variables
            // TODO: Does not work because due to same reason. Also, programs like Pi violate the rule stated in the paper (reading and writing to same species)
    
    if not(comparisonAndConditionalExecutionAreSeparated crn) then
        Result.Error "Comparison module and conditional statements cannot be used in the same step"
    //else if not(noReadingAndWritingToSameSpecies crn) then
    //    Result.Error "Can't read and write to the same species in the same step."
    else
        Result.Ok crn

/// Analyze a CRN for semantic errors and return either an error or the program if everything is fine
let analyze (crn: Crn) : Result<Crn, string> =
    let crn = { crn with Arguments = extractMissingArguments crn }
    
    let conflictingResult = statementsAreNotConflicting crn
    
    if not(concentrationsAreDefinedBeforeSteps crn) then
        Result.Error "All concentration declarations must happen before any steps."
    else if not(moduleOutputSpeciesAreDifferentFromInput crn) then
        Result.Error "Module statements must not have an output that is the same as an input."
    else if (match conflictingResult with | Error _ -> true | Ok _ -> false) then
        conflictingResult
    else
        Result.Ok crn