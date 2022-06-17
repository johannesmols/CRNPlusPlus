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
    
/// Check the entire program for the species that are initially defined with a concentration,
/// and find any that are used in the program that are used, but never defined.
/// In the paper, such variables are initialized to 0. We require the specific declaration of them beforehand.
let getUsedSpeciesThatWereNotDefinedAndThatAreDefinedInModules crn =
    let rec getSpeciesUsedInCommand = function
        | ModuleStmt m ->
            match m with
            | Load(SpeciesLiteral s1, SpeciesLiteral s2)
            | SquareRoot(SpeciesLiteral s1, SpeciesLiteral s2) -> [s1; s2], [s1]
            | Compare(SpeciesLiteral s1, SpeciesLiteral s2) -> [s1; s2], []
            | Add(SpeciesLiteral s1, SpeciesLiteral s2, SpeciesLiteral s3)
            | Subtract(SpeciesLiteral s1, SpeciesLiteral s2, SpeciesLiteral s3)
            | Multiply(SpeciesLiteral s1, SpeciesLiteral s2, SpeciesLiteral s3)
            | Divide(SpeciesLiteral s1, SpeciesLiteral s2, SpeciesLiteral s3) -> [s1; s2; s3], [s3]
            | _ -> [], []
        | ConditionalStmt c ->
            match c with
            | IfGreaterThan c | IfGreaterThanOrEquals c | IfEquals c | IfLesserThan c | IfLesserThanOrEquals c ->
                let a, b = c |> List.map getSpeciesUsedInCommand |> List.unzip
                a |> List.concat, b |> List.concat
    
    let defined = crn.Statements |> List.choose (fun s ->
        match s with
        | ConcentrationStmt(SpeciesLiteral c, _) -> Some c
        | _ -> None) |> Set.ofList
    
    let used, definedLater =
        crn.Statements
        |> List.choose (fun s ->
            match s with
            | StepStmt cmds ->
                let a, b = cmds |> List.map getSpeciesUsedInCommand |> List.unzip
                Some (a |> List.concat, b |> List.concat)
            | _ -> None) |> List.unzip |> fun (a, b) -> a |> List.concat |> Set.ofList, b |> List.concat |> Set.ofList
    
    let defined = Set.union defined definedLater
    Set.difference used defined
    
let statementsAreNotConflicting crn =
    true // TODO

/// Analyze a CRN for semantic errors and return either an error or the program if everything is fine
let analyze (crn: Crn) : Result<Crn, string> =
    let crn = { crn with Arguments = extractMissingArguments crn }
    
    let undefinedSpecies = getUsedSpeciesThatWereNotDefinedAndThatAreDefinedInModules crn
    
    if not(concentrationsAreDefinedBeforeSteps crn) then
        Result.Error "All concentration declarations must happen before any steps."
    else if not(moduleOutputSpeciesAreDifferentFromInput crn) then
        Result.Error "Module statements must not have an output that is the same as an input."
    else if not undefinedSpecies.IsEmpty then
        Result.Error $"The following species were used in statements but were not defined with an initial concentration: %A{undefinedSpecies}"
    else if not(statementsAreNotConflicting crn) then
        Result.Error "Statements within a step module are conflicting."
    else
        Result.Ok crn