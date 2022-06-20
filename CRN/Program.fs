// Johannes Mols, 15-06-2022

open System
open System.IO

open CRN.Core.CRNPP
open CRN.Core.CRNPP.Parser
open CRN.Core.CRNPP.Interpreter

let argToVal (input: string) =
    let split = input.Split('=', 2) |> List.ofArray
    match split with
    | species::[value] -> species, value |> float
    | _ -> failwith $"Argument {input} cannot be split in two parts by the character '=' and parsed to species and float literals."

let parseProgramFromFile file (args: string array) =
    try
        let text = File.ReadAllText file
        parse text
    with
    | :? IOException as e -> Result.Error $"Exception occurred while reading input file: {e.Message}"
    | e -> Result.Error $"Exception occurred: {e.Message}"

[<EntryPoint>]
let main args =
    if args.Length < 2 then
        printfn "Too few arguments provided. Please provide the source file name, number of steps to simulate, and required values, if any."
        Environment.Exit 1
    
    let input_file = args[0]
    if not(File.Exists input_file) then
        printfn $"{Environment.CurrentDirectory}"
        printfn $"Input file '{input_file}' does not exist"
        Environment.Exit 2
    
    let parseRes = parseProgramFromFile input_file args
    match parseRes with
    | Error err ->
        printfn $"{err}"
        Environment.Exit 3
    | Ok crn ->
        if args.Length <> crn.Arguments.Length + 2 then
            failwith $"Parsed program expects values for the following species: %A{crn.Arguments}"
        let allStates = interpret crn (args |> List.ofArray |> List.skip 2 |> List.map argToVal |> Map.ofList)
        let stepsToEvaluate = args[1] |> int
        let states = allStates |> Seq.take stepsToEvaluate
        //states |> Seq.iteri (fun i s -> printfn $"State {i}: %A{s}")
        printfn $"Plotting {states |> Seq.length} states and displaying it in the browser..."
        Plot.plot states
        
    0
   