// Patrikas Balsys, 21-06-2022

module CRN.Simulation.ReactionSimulator

open CRN.Core.Types.Parser

val simulate : float -> int -> Crn -> Map<string, float> -> seq<Map<string, float>>