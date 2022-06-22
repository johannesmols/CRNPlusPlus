// Johannes Mols, 15-06-2022
// Patrikas Balsys, 20-06-2022: Added reaction statements and types

module CRN.Core.Types

// Parser types

module Parser =
    type Literal =
        | SpeciesLiteral of string
        | FloatLiteral of float
        
    type ReactionStmt =
        | Reaction of Literal list * Literal list * Literal

    type ModuleStmt =
        | Load of Literal * Literal
        | Add of Literal * Literal * Literal
        | Subtract of Literal * Literal * Literal
        | Multiply of Literal * Literal * Literal
        | Divide of Literal * Literal * Literal
        | SquareRoot of Literal * Literal
        | Compare of Literal * Literal
        
    type ConditionalStmt =
        | IfGreaterThan of Command list
        | IfGreaterThanOrEquals of Command list
        | IfEquals of Command list
        | IfLesserThan of Command list
        | IfLesserThanOrEquals of Command list
        
    and Command =
        | ConditionalStmt of ConditionalStmt
        | ModuleStmt of ModuleStmt
        | ReactionStmt of ReactionStmt

    type Statement =
        | ConcentrationStmt of Literal * Literal
        | StepStmt of Command list 

    type Crn = {
        Statements : Statement list
        Arguments: string list
    }

// Simulator types
module Simulator =
    type State = {
        Concentrations : Map<string, float>
        Comparison : float * float
    } with
        member this.IsEqual = abs(fst this.Comparison - snd this.Comparison) <= 0.5
        member this.IsGreater = fst this.Comparison > snd this.Comparison + 0.5
        member this.IsLesser = fst this.Comparison < snd this.Comparison - 0.5
        member this.IsGreaterOrEquals = this.IsEqual || this.IsGreater
        member this.IsLesserOrEquals = this.IsEqual || this.IsLesser
        
module ReactionSimulator =
    let defaultPrecision = 0.001
    let defaultStepTime = 20
    
    // Reaction: reactants, products, reaction rate
    type Rxn = Rxn of string list * string list * float
    // Comparison statement: species, species
    type Cmp = Cmp of string * string
    // Step: default Rxns, IfGT Rxns, IfGE Rxns, IfEQ Rxns, IfLT Rxns, IfLE Rxns, Cmp
    type Step = Step of Rxn list * Rxn list * Rxn list * Rxn list * Rxn list * Rxn list * Option<Cmp>
        