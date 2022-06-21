// Johannes Mols, 15-06-2022

module CRN.Core.Parser

open CRN.Core.Types.Parser

val parse : string -> Result<Crn, string>