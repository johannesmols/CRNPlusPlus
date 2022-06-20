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


// let crnLd = File.ReadAllText "./CRN/Scripts/examples/basic/ld.crnpp"
// let crnAdd = File.ReadAllText "./CRN/Scripts/examples/basic/add.crnpp"
// let crnSub = File.ReadAllText "./CRN/Scripts/examples/basic/sub.crnpp"
// let crnMul = File.ReadAllText "./CRN/Scripts/examples/basic/mul.crnpp"
// let crnDiv = File.ReadAllText "./CRN/Scripts/examples/basic/div.crnpp"
// let crnSqrt = File.ReadAllText "./CRN/Scripts/examples/basic/sqrt.crnpp"

let crnOsc = File.ReadAllText "./CRN/Scripts/examples/oscillator.crnpp"
let crnSeq = File.ReadAllText "./CRN/Scripts/examples/sequence.crnpp"
let crnCou = File.ReadAllText "./CRN/Scripts/examples/counter.crnpp"
let crnGcd = File.ReadAllText "./CRN/Scripts/examples/gcd.crnpp"

plotReaction 0.001 20 200 crnCou (Map [ ("a0", 3.0) ])