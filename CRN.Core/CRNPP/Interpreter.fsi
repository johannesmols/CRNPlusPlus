module CRN.Core.CRNPP.Interpreter

open CRN.Core.CRNPP.Types

type State = {
    Concentrations : Map<string, float>
    Comparison : float * float
}

val interpret : Crn -> Map<string, float> -> bool