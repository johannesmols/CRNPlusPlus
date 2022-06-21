// Johannes Mols, 15-06-2022

module CRN.Core.SemanticAnalyzer

open CRN.Core.Types.Parser

val analyze : Crn -> Result<Crn, string>