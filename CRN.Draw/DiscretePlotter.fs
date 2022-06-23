// Johannes Mols, 15-06-2022

module CRN.Draw.DiscretePlotter

open CRN.Core.Types.Simulator

open Plotly.NET

/// Extract the different series of data from a sequence of states
let getDataSeries states =
    let series =
        states
        |> Seq.map (fun s -> s.Concentrations)
        |> Seq.fold (fun (acc: Map<string, float list>) s ->
                (acc, s) ||> Map.fold (fun state key value ->
                    if state.ContainsKey key then
                        state.Add(key, state[key] @ [value]) 
                    else
                        state.Add(key, [value]))
            )
            Map.empty
    
    let maxLength = series
                    |> Map.toList
                    |> List.maxBy (fun (_, l) -> l.Length)
                    |> snd
                    |> List.length
    
    series
    |> Map.map (fun _ v ->
       match v.Length = maxLength with
       | false -> List.replicate (maxLength - v.Length) 0. @ v
       | true -> v)
    |> Map.toSeq
    
/// Create lines with hard edges by adding a vertical line to the y-value of the next value after each value
let interpolateLines data =
    data
    |> Seq.map (fun (label, data) ->
        label,
        data
        |> Seq.indexed
        |> Seq.pairwise
        |> Seq.map (fun ((ax, ay), (bx, _)) -> [float ax, ay; float bx, ay])
        |> Seq.concat)

/// Plot a sequence of states and show it in the browser
let plot states =
    let data = getDataSeries states
    let interpolated = interpolateLines data
    
    let charts = interpolated |> Seq.map (fun (name, values) ->
        Chart.Line(xy = values, Name = name))
    
    charts
    |> Chart.combine
    |> Chart.show