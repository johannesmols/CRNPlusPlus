#load "./ReactionSimulator.fsx"
#r "nuget: Plotly.NET, 2.0.0"

open ReactionSimulator
open Plotly.NET
open System.IO

let rec toCharts' xs (values: list<Map<string, float>>) speciesList =
    match speciesList with
    | [] -> []
    | species :: tail ->
        let speciesSequence = List.map (fun m -> Map.find species m) values

        Chart.Line(xs, speciesSequence, Name = species)
        :: toCharts' xs values tail

let toCharts prec maxTime (s: seq<Map<string, float>>) =
    let values = Seq.toList s
    let firstPoint = List.item 0 values
    let speciesList = Seq.toList (Map.keys firstPoint)
    let xs = [ 0.0 .. (prec) .. float (maxTime) ]
    toCharts' xs values speciesList

let plotReaction prec stepTime maxTime crn =
    simulate prec stepTime crn
    |> Seq.take (maxTime * int (1.0 / prec))
    |> toCharts prec maxTime
    |> Chart.combine
    |> Chart.show

let crnpp1 = File.ReadAllText "./CRN/Scripts/examples/multiplication.crnpp"
let crnpp2 = File.ReadAllText "./CRN/Scripts/examples/oscillator.crnpp"
let crnpp3 = File.ReadAllText "./CRN/Scripts/examples/sequence.crnpp"

plotReaction 0.001 20 100 crnpp3