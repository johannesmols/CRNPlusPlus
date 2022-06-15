module CRN.Core.CRNPP.SemanticAnalyzer

open CRN.Core.CRNPP.Types

val analyze : Crn -> Result<Crn, string>