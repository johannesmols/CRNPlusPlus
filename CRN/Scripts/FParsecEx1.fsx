// Michael R. Hansen   11-06-2022
// A parser for simple expressions


#r "nuget: FParsec"
#nowarn "40"

open FParsec

type E =
    | V of string
    | C of int
    | Sub of E * E

(*
E -> V | C | ( E ) | E - E
where V is an identifier, C is an integer
*)


// For the lexical part
// skip blanks after a token
let token p = p .>> spaces

let symbol s = token (pstring s)

let pinteger: Parser<int, unit> = token pint32

let ident: Parser<string, unit> =
    let identifierOrChar c = isLetter c || isDigit c
    token (many1Satisfy2L isLetter identifierOrChar "identifier")

// for the Grammer part
let pV =
    parse {
        let! x = ident
        return V x
    }

let pC =
    parse {
        let! n = pinteger
        return C n
    }

// A grammar without left recursion and with disjoint first sets of choices
//
// T    -> V | C | ( E )
// E    -> T Eopt
// Eopt -> - T Eopt | epsilon

// A formulation based on computation expressions

let rec pT =
    pV
    <|> pC
    <|> parse {
        let! _ = symbol "("
        let! e = pE
        let! _ = symbol ")"
        return e
    }

and pE =
    parse {
        let! e = pT
        return! pEopt e
    }

and pEopt e =
    parse {
        let! _ = symbol "-"
        let! e' = pT
        return! pEopt (Sub(e, e'))
    }
    <|> preturn e


// remember to skip leading spaces when parsing an expression

let parseE str = run (spaces >>. pE) str

let t1 = parseE "   1-x"

let t2 = parseE "   1-x-   2 "

let t3 =
    parseE
        "   1- ((xjhde -   2) 
  - y)
"

// A formulation based directly on combinators

(*
let pV = ident >>= fun x -> preturn (V x);;

let pC = pinteger >>= fun n -> preturn (C n);;

let rec pT = pV
          <|> pC
          <|> between (symbol "(") (symbol ")") pE

// computation expression to achieve laziness
and pE = parse { let! e = pT
                 return! pEopt e}

and pEopt e = symbol "-" >>. (pT >>= fun e' -> pEopt(Sub(e,e')))
           <|> preturn e;;
*)


// A formulation using chainl1
(*
let rec pT = pV
             <|> pC
             <|> between (symbol "(") (symbol ")") pE

// computation expression to achieve laziness
and pE = parse {return! chainl1 pT pMinus}

and pMinus = symbol "-" >>. preturn (fun e e' -> Sub(e,e'));;
*)