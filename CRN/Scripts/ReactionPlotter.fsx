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

let plotReaction prec stepTime maxTime crn args =
    simulate prec stepTime crn args
    |> Seq.take (maxTime * int (1.0 / prec))
    |> toCharts prec maxTime
    |> Chart.combine
    |> Chart.show

// --- Testing

let crnCou = File.ReadAllText "./CRN/Scripts/examples/counter.crnpp"
let crnDiv = File.ReadAllText "./CRN/Scripts/examples/division.crnpp"
let crnEul = File.ReadAllText "./CRN/Scripts/examples/euler.crnpp"
let crnFac = File.ReadAllText "./CRN/Scripts/examples/factorial.crnpp"
let crnGcd = File.ReadAllText "./CRN/Scripts/examples/gcd.crnpp"
let crnOsc = File.ReadAllText "./CRN/Scripts/examples/oscillator.crnpp"
let crnPi = File.ReadAllText "./CRN/Scripts/examples/pi.crnpp"
let crnSeq = File.ReadAllText "./CRN/Scripts/examples/sequence.crnpp"
let crnSqu = File.ReadAllText "./CRN/Scripts/examples/squareroot.crnpp"

plotReaction 0.010 20 200 crnCou (Map [ ("a0", 3) ])
plotReaction 0.010 20 500 crnDiv (Map [ ("a0", 20); ("b0", 3) ])
plotReaction 0.001 20 200 crnEul (Map [])
plotReaction 0.010 20 200 crnFac (Map [ ("f0", 5) ])
plotReaction 0.010 20 200 crnGcd (Map [ ("a0", 32); ("b0", 12) ])
plotReaction 0.001 20 200 crnOsc (Map [])
plotReaction 0.001 20 200 crnPi (Map [])
plotReaction 0.010 20 200 crnSeq (Map [])
plotReaction 0.010 20 200 crnSqu (Map [ ("n0", 10) ])