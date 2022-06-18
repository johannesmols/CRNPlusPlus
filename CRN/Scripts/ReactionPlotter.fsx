#load "./ReactionSimulator.fsx"
#r "nuget: Plotly.NET, 2.0.0"

open ReactionSimulator
open Plotly.NET
open System.IO

let xs = [ 0.0..0.001..15.0 ]

let rec toCharts' (values: list<Map<string, float>>) speciesList =
    match speciesList with
    | [] -> []
    | species :: tail ->
        let speciesSequence = List.map (fun m -> Map.find species m) values

        Chart.Line(xs, speciesSequence, Name = species)
        :: toCharts' values tail

let toCharts (s: seq<Map<string, float>>) =
    let firstPoint = Seq.item 0 s
    let speciesList = Seq.toList (Map.keys firstPoint)
    toCharts' (Seq.toList s) speciesList

let plotReaction s =
    toCharts s |> Chart.combine |> Chart.show


let crnpp1 = File.ReadAllText "./CRN/Scripts/examples/multiplication.crnpp"
let crnpp2 = File.ReadAllText "./CRN/Scripts/examples/oscillator.crnpp"

trySimulate crnpp1
|> Seq.take (15 * 1000)
|> plotReaction

trySimulate crnpp2
|> Seq.take (15 * 1000)
|> plotReaction