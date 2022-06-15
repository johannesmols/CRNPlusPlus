module CRN.Core.CRNPP.Parser

open CRN.Core.CRNPP.Types

val parse : string -> Result<Crn, string>