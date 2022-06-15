open System
open System.IO

open CRN.Core

[<EntryPoint>]
let main args =
    if args.Length <> 1 then
        printfn $"Too many or too little arguments (provided {args.Length}, expected 1)"
        Environment.Exit 1
    
    let input_file = args[0]
    if not(File.Exists input_file) then
        printfn $"{Environment.CurrentDirectory}"
        printfn $"Input file '{input_file}' does not exist"
        Environment.Exit 2
    
    try
        let text = File.ReadAllText input_file
        match parse text with
        | Result.Ok res -> printfn $"Success: {res}"
        | Result.Error err -> printfn $"Failure: {err}"
    with
    | e -> printfn $"Exception occurred during parsing: {e.Message}"
    
    0
   