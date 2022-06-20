// Johannes Mols, 15-06-2022
// Patrikas Balsys, 20-06-2022: Added reaction statements

module CRN.Core.CRNPP.Types

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