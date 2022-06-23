// Johannes Mols, 15-06-2022

module CRN.Simulation.DiscreteSimulator

open CRN.Core.Types.Parser
open CRN.Core.Types.Simulator

val simulate : Crn -> Map<string, float> -> seq<State>