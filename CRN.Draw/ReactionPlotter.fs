// Patrikas Balsys, 21-06-2022

module CRN.Draw.ReactionPlotter

open Plotly.NET

// Constructs charts from a list of mapped values
let rec toCharts' xs (values: list<Map<string, float>>) speciesList =
    match speciesList with
    | [] -> []
    | species :: tail ->
        let speciesSequence = List.map (fun m -> Map.find species m) values

        Chart.Line(xs, speciesSequence, Name = species)
        :: toCharts' xs values tail

// Constructs charts for all species in the reaction
let toCharts prec maxTime (s: seq<Map<string, float>>) =
    let values = Seq.toList s
    let firstPoint = List.item 0 values
    let speciesList = Seq.toList (Map.keys firstPoint)
    let xs = [ 0.0 .. (prec) .. float (maxTime) ]
    toCharts' xs values speciesList

// Plots a reaction sequence
let plotReaction prec maxTime states =
    states
    |> Seq.take (maxTime * int (1.0 / prec))
    |> toCharts prec maxTime
    |> Chart.combine
    |> Chart.show