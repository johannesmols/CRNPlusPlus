// Patrikas Balsys, 21-06-2022

module CRN.Draw.ReactionPlotter

open CRN.Core.Types
open Plotly.NET

// Takes every n'th value of a sequence, used to trim down the chart size
let everyNth n sequence =
    sequence
    |> Seq.mapi (fun i e -> if i % n = n - 1 then Some(e) else None)
    |> Seq.choose id

// Constructs charts from a list of mapped values
let rec toCharts' xs (values: list<Map<string, float>>) speciesList =
    match speciesList with
    | [] -> []
    | species :: tail ->
        let speciesSequence = List.map (Map.find species) values

        Chart.Line(xs, speciesSequence, Name = species)
        :: toCharts' xs values tail

// Constructs charts for all species in the reaction
let toCharts prec maxTime (s: seq<Map<string, float>>) =
    let nth = int (1.0 / prec) / 10
    let values = Seq.toList (everyNth nth s)
    let firstPoint = List.item 0 values
    let speciesList = Seq.toList (Map.keys firstPoint)
    let xs = [ 0.0 .. 0.1 .. float (maxTime) ]
    toCharts' xs values speciesList

// Plots a reaction sequence
let plotReaction prec maxTime states =
    states
    |> Seq.take (maxTime * int (1.0 / prec))
    |> toCharts prec maxTime
    |> Chart.combine
    |> Chart.show

// Plotting with default parameters, only need to define the number of steps
let plotReactionDefault stepCount states =
    states
    |> Seq.take (ReactionSimulator.defaultStepTime * stepCount * 1000)
    |> toCharts ReactionSimulator.defaultPrecision (ReactionSimulator.defaultStepTime * stepCount)
    |> Chart.combine
    |> Chart.show