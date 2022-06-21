// Johannes Mols, 21-06-2022

open CRN.Core
open CRN.Core.Parser
open CRN.Simulation
open CRN.Draw

open System
open System.IO

open CommandLine

[<Verb("parse", HelpText = "Only parse a program, do not simulate it.")>]
type OnlyParseOptions = {
    [<Option('f', "file", Required = true, HelpText = "The path and file name of the input CRN++ program")>] file : string
    [<Option('a', "analyze", Required = false, HelpText = "Also analyze the parsed program for semantic errors")>] analyze : bool
}

[<Verb("simulate", HelpText = "Parse a program, simulate it, and draw a chart with the results.")>]
type SimulatorOptions = {
    [<Option('f', "file", Required = true, HelpText = "The path and file name of the input CRN++ program")>] file : string
    [<Option('t', "take", Required = true, HelpText = "How many steps to simulate")>] take : int
    [<Option('s', "species", Required = false, HelpText = "Missing species")>] species : string seq
    [<Option('v', "values", Required = false, HelpText = "Values for missing species")>] values : float seq
    [<Option('r', "reaction", Required = false, HelpText = "Use the reaction simulator instead of the regular one (requires precision and step time arguments)")>] reaction : bool
    [<Option('p', "precision", Required = false, HelpText = "Precision of reaction simulator")>] precision : float
    [<Option('e', "step time", Required = false, HelpText = "Step time of reaction simulator")>] stepTime : int
}

/// Parse a CRN++ program from file on disk
let parseProgramFromFile file =
    if not(File.Exists file) then
        Result.Error $"Input file '{file}' does not exist"
    else
        try
            let text = File.ReadAllText file
            parse text
        with
        | :? IOException as e -> Result.Error $"Exception occurred while reading input file: {e.Message}"
        | e -> Result.Error $"Exception occurred: {e.Message}"

[<EntryPoint>]
let main args =
    let argsRes = CommandLine.Parser.Default.ParseArguments<OnlyParseOptions, SimulatorOptions> args
    match argsRes with
    | :? Parsed<obj> as cmd ->
        match cmd.Value with
        | :? OnlyParseOptions as opts ->
            let parseRes = parseProgramFromFile opts.file
            match parseRes with
            | Ok crn ->
                printfn $"%A{crn}"
                if opts.analyze then
                    let analyzed = SemanticAnalyzer.analyze crn
                    match analyzed with
                    | Ok _ -> printfn "Analyzed program and found no issues."
                    | Error err -> eprintfn $"Analyzed program and found the following issue: {err}"
            | Error err -> eprintfn $"Parsing error: {err}"
        | :? SimulatorOptions as opts ->
            let parseRes = parseProgramFromFile opts.file
            match parseRes with
            | Ok crn ->
                let analyzeRes = SemanticAnalyzer.analyze crn
                match analyzeRes with
                | Ok crn ->
                    try
                        let args = Seq.zip opts.species opts.values |> Map.ofSeq
                        if opts.reaction then
                            let states = ReactionSimulator.simulate opts.precision opts.stepTime crn args
                            printfn $"Successfully simulated {opts.take} states using the reaction simulator. Plotting them and opening it in the browser..."
                            ReactionPlotter.plotReactionDefault opts.take states
                        else
                            let states = Simulator.simulate crn args |> Seq.take opts.take
                            printfn $"Successfully simulated {opts.take} states. Plotting them and opening it in the browser..."
                            Plotter.plot states
                    with
                    | e -> eprintfn $"Error: {e.Message}"
                | Error err -> eprintfn $"The parsed program has a semantic issue: {err}"
            | Error err -> eprintfn $"Parsing error: {err}"
        | _ -> Environment.Exit 1
    | :? NotParsed<obj> | _ -> Environment.Exit 1
        
    0
   