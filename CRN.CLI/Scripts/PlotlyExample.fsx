// Michael R. Hansen   03-06-2022

#r "nuget: Plotly.NET, 2.0.0";;

open Plotly.NET
open System


open Plotly.NET.LayoutObjects // this namespace contains all object abstractions for layout styling


let mirroredXAxis =
    LinearAxis.init(
//        Title = Title.init(Text="Mirrored axis"),
        ShowLine = false,
        Mirror = StyleParam.Mirror.False,
        ShowGrid = false
//        Ticks = StyleParam.TickOptions.Inside
    )

let mirroredYAxis = 
    LinearAxis.init(
//        Title = Title.init(Text="Log axis"),
//        AxisType = StyleParam.AxisType.Log,
        ShowLine = false,
        Mirror = StyleParam.Mirror.False,
        ShowGrid = false
    )

let test() = 
   [ Chart.Point([(0,4.5)], MultiText=["A Point"], MultiTextPosition=[StyleParam.TextPosition.TopCenter], ShowLegend = false);
     Chart.Line([-3.0;3.0], [4.0;4.0] ,LineColor = Color.fromString "green", ShowLegend = false);
     Chart.Line([-3.0;3.0], [2.0;2.0] ,LineColor = Color.fromString "yellow", ShowLegend = false);
     Chart.Line([-3.0;-3.0], [4.0; 2.0],LineColor = Color.fromString "red", ShowLegend = false);
     Chart.Line([3.0;3.0], [4.0;2.0],LineColor = Color.fromString "blue", ShowLegend = false)  
   ] 
   |> Chart.combine 
   |> Chart.withXAxis mirroredXAxis
   |> Chart.withYAxis mirroredYAxis
   |> Chart.show;; 

test();;