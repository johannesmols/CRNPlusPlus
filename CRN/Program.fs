open System
open System.IO

open CRN.Core.CRNPP.Parser
open CRN.Core.CRNPP.Interpreter

let argToVal (input: string) =
    let split = input.Split('=', 2) |> List.ofArray
    match split with
    | species::[value] -> species, value |> float
    | _ -> failwith $"Argument {input} cannot be split in two parts by the character '=' and parsed to species and float literals."

[<EntryPoint>]
let main args =
    if args.Length = 0 then
        printfn "No arguments provided. Please provide the source file name and required values, if any."
        Environment.Exit 1
    
    let input_file = args[0]
    if not(File.Exists input_file) then
        printfn $"{Environment.CurrentDirectory}"
        printfn $"Input file '{input_file}' does not exist"
        Environment.Exit 2
    
    try
        let text = File.ReadAllText input_file
        match parse text with
        | Result.Ok res ->
            printfn $"Successfully parsed {Path.GetFileName input_file} into AST: {res}"
            if args.Length <> res.Arguments.Length + 1 then
                failwith $"Parsed program expects values for the following species: {res.Arguments}"
            let interpretation = interpret res (args |> List.ofArray |> List.tail |> List.map argToVal |> Map.ofList)
            interpretation |> ignore
        | Result.Error err -> printfn $"Failed to parse {Path.GetFileName input_file}: {err}"
    with
    | e -> printfn $"Exception occurred during parsing: {e.Message}"
    
    0
   