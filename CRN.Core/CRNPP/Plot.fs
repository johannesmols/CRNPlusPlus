module CRN.Core.CRNPP.Plot

open CRN.Core.CRNPP.Interpreter
open CRN.Core.CRNPP.Types

open Plotly.NET

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

let plot (states: seq<State>) =
    let data = getDataSeries states
    
    let xData = [0. .. 10.]
    let yData = [0. .. 10.]
    Chart.Point(xData,yData)
    |> Chart.show