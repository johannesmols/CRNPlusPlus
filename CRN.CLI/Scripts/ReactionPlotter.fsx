// Patrikas Balsys, 21-06-2022

#load "./ReactionSimulator.fsx"
#r "nuget: Plotly.NET, 2.0.0"

open ReactionSimulator
open Plotly.NET
open System.IO

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
        let speciesSequence = List.map (fun m -> Map.find species m) values

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
let plotReaction prec stepTime maxTime crn args =
    simulate prec stepTime crn args
    |> Seq.take (maxTime * int (1.0 / prec))
    |> toCharts prec maxTime
    |> Chart.combine
    |> Chart.show

// Plotting with default parameters, only need to define the number of steps
let plotReactionDefault stepCount crn args =
    let prec = 0.001
    let stepTime = 20

    simulate prec 20 crn args
    |> Seq.take (stepTime * stepCount * 1000)
    |> toCharts prec (stepTime * stepCount)
    |> Chart.combine
    |> Chart.show

// --- Testing

let crnCou = File.ReadAllText "./CRN.CLI/Scripts/examples/counter.crnpp"
let crnDiv = File.ReadAllText "./CRN.CLI/Scripts/examples/division.crnpp"
let crnEul = File.ReadAllText "./CRN.CLI/Scripts/examples/euler.crnpp"
let crnFac = File.ReadAllText "./CRN.CLI/Scripts/examples/factorial.crnpp"
let crnGcd = File.ReadAllText "./CRN.CLI/Scripts/examples/gcd.crnpp"
let crnOsc = File.ReadAllText "./CRN.CLI/Scripts/examples/oscillator.crnpp"
let crnPi = File.ReadAllText "./CRN.CLI/Scripts/examples/pi.crnpp"
let crnSeq = File.ReadAllText "./CRN.CLI/Scripts/examples/sequence.crnpp"
let crnSqu = File.ReadAllText "./CRN.CLI/Scripts/examples/squareroot.crnpp"

plotReaction 0.010 20 200 crnCou (Map [ ("a0", 3) ])
plotReaction 0.010 20 500 crnDiv (Map [ ("a0", 20); ("b0", 3) ])
plotReaction 0.001 20 200 crnEul (Map [])
plotReaction 0.010 20 200 crnFac (Map [ ("f0", 5) ])
plotReaction 0.010 20 200 crnGcd (Map [ ("a0", 32); ("b0", 12) ])
plotReaction 0.001 20 200 crnOsc (Map [])
plotReaction 0.001 20 200 crnPi (Map [])
plotReaction 0.010 20 200 crnSeq (Map [])
plotReaction 0.010 20 200 crnSqu (Map [ ("n0", 10) ])

plotReactionDefault 10 crnGcd (Map [ ("a0", 32); ("b0", 12) ])