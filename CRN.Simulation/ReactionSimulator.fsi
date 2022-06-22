// Patrikas Balsys, 21-06-2022

module CRN.Simulation.ReactionSimulator

open CRN.Core.Types.Parser
open CRN.Core.Types.ReactionSimulator

val convertCRN : Crn -> Statement list * Step list
val simulate : float -> int -> Crn -> Map<string, float> -> seq<Map<string, float>>