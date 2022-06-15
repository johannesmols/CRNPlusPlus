module CRN.Core

type Literal =
    | SpeciesLiteral of string
    | FloatLiteral of float

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

type Statements =
    | ConcentrationStmt of Literal * Literal
    | StepStmt of Command list 

type Crn = {
    Statements : Statements list
}

val parse : string -> Result<Crn, string>