module CRN.Core.CRNPP.Interpreter

open CRN.Core.CRNPP.Types

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

let interpret crn (args: Map<string, float>) =
    if crn.Arguments.Length <> args.Count then
        failwith $"Expected {crn.Arguments.Length} arguments, but received {args.Count}."
        
    let crn = replaceConcentrationsWithArguments crn args
        
    true