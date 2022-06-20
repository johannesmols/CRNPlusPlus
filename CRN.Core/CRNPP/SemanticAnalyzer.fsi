// Johannes Mols, 15-06-2022

module CRN.Core.CRNPP.SemanticAnalyzer

open CRN.Core.CRNPP.Types

val analyze : Crn -> Result<Crn, string>