// Patrikas Balsys 14-06-2022
// Based on the Tutorial section of the FParsec documentation

// 4.1

#r "nuget: FParsec"

open FParsec

// 4.10

type UserState = unit
type Parser<'t> = Parser<'t, UserState>
let someParser: Parser<_> = pstring "[" >>. pfloat .>> pstring "]"

// 4.2 Parsing a single float

let test p str =
    match run p str with
    | Success (result, _, _) -> printfn "Success: %A" result
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pfloat "1.25"
test pfloat "1.25E 3"
test pfloat "1.25E3"

// 4.3 Parsing a float between brackets

let str s = pstring s
let floatBetweenBrackets: Parser<_> = str "[" >>. pfloat .>> str "]"

test floatBetweenBrackets "[1.25]"
test floatBetweenBrackets "[]"
test floatBetweenBrackets "[1.25"

// 4.4 Abstracting parsers

// let between pBegin pEnd p = pBegin >>. p .>> pEnd
let betweenStrings s1 s2 p = between (str s1) (str s2) p

// let floatBetweenBrackets2 = pfloat |> betweenStrings "[" "]"
let floatBetweenDoubleBrackets = pfloat |> betweenStrings "[[" "]]"

test floatBetweenDoubleBrackets "[[1.25]]"

// 4.5 Parsing a list of floats

test (many floatBetweenBrackets) "(1)"
test (many floatBetweenBrackets) "[1.25]"
test (many floatBetweenBrackets) "[2][3.2][4]"

test (many1 floatBetweenBrackets) "(1)"

let floatList = str "[" >>. sepBy pfloat (str ",") .>> str "]"

test floatList "[1.0]"
test floatList "[1,2,3]"
test floatList "[1,]"

// 4.6 Handling whitespace

let ws = spaces

let str_ws s = pstring s .>> ws
let float_ws = pfloat .>> ws

let numberList =
    str_ws "[" >>. sepBy float_ws (str_ws ",")
    .>> str_ws "]"

test numberList "[   1 ,  2 ]   "

let numberListFile = ws >>. numberList .>> eof

// 4.7 Parsing string data

test (many (str "a" <|> str "b")) "abba"

test (skipStringCI "<float>" >>. pfloat) "<FLOAT>1.0"

let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    .>> ws

test identifier "_"
test identifier "_test1="
test identifier "1"
test identifier "_2"

let stringLiteral =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')

    let unescape c =
        match c with
        | 'n' -> '\n'
        | 'r' -> '\r'
        | 't' -> '\t'
        | c -> c

    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"") (manyChars (normalChar <|> escapedChar))

test stringLiteral "\"abc\""
test stringLiteral "\"abc\\\"def\\\\ghi\""
test stringLiteral "\"abc\\tdef\""

let stringLiteral2 =
    let normalCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')

    let escapedChar =
        pstring "\\"
        >>. (anyOf "\\nrt\""
             |>> function
                 | 'n' -> "\n"
                 | 'r' -> "\r"
                 | 't' -> "\t"
                 | c -> string c)

    between (pstring "\"") (pstring "\"") (manyStrings (normalCharSnippet <|> escapedChar))

test stringLiteral2 "\"abc\""

let stringLiteral3 =
    let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')

    let escapedChar =
        pstring "\\"
        >>. (anyOf "\\nrt\""
             |>> function
                 | 'n' -> "\n"
                 | 'r' -> "\r"
                 | 't' -> "\t"
                 | c -> string c)

    between (pstring "\"") (pstring "\"") (stringsSepBy normalCharSnippet escapedChar)

test stringLiteral3 "\"abc\""

// 4.8 Sequentially applying parsers

let product = pipe2 float_ws (str_ws "*" >>. float_ws) (fun x y -> x * y)

test product "3 * 5"

type StringConstant = StringConstant of string * string

let stringConstant =
    pipe3 identifier (str_ws "=") stringLiteral (fun id _ str -> StringConstant(id, str))

test stringConstant "myString = \"stringValue\""

test (float_ws .>>. (str_ws "," >>. float_ws)) "123, 456"

// 4.9 Parsing alternatives

let boolean =
    (stringReturn "true" true)
    <|> (stringReturn "false" false)

test boolean "false"

test ((ws >>. str "a") <|> (ws >>. str "b")) " b"
test (ws >>. (str "a" <|> str "b")) " b"

test (choice [ str "a"; str "b"; str "c" ]) "c"

// 4.11 Parsing JSON

type Json =
    | JString of string
    | JNumber of float
    | JBool of bool
    | JNull
    | JList of Json list
    | JObject of Map<string, Json>

let jnull = stringReturn "null" JNull
let jtrue = stringReturn "true" (JBool true)
let jfalse = stringReturn "false" (JBool false)

let jnumber = pfloat |>> JNumber

let jstringLiteral =
    let escape =
        anyOf "\"\\/bfnrt"
        |>> function
            | 'b' -> "\b"
            | 'f' -> "\u000C"
            | 'n' -> "\n"
            | 'r' -> "\r"
            | 't' -> "\t"
            | c -> string c // every other char is mapped to itself

    let unicodeEscape =
        /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
        let hex2int c = (int c &&& 15) + (int c >>> 6) * 9

        str "u"
        >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            (hex2int h3) * 4096
            + (hex2int h2) * 256
            + (hex2int h1) * 16
            + hex2int h0
            |> char
            |> string)

    let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
    let normalCharSnippet = manySatisfy (fun c -> c <> '"' && c <> '\\')

    between (str "\"") (str "\"") (stringsSepBy normalCharSnippet escapedCharSnippet)

let jstring = jstringLiteral |>> JString

let jvalue, jvalueRef = createParserForwardedToRef<Json, unit> ()

let listBetweenStrings sOpen sClose pElement f =
    between
        (str sOpen)
        (str sClose)
        (ws >>. sepBy (pElement .>> ws) (str "," >>. ws)
         |>> f)

let jlist = listBetweenStrings "[" "]" jvalue JList

let keyValue =
    stringLiteral
    .>>. (ws >>. str ":" >>. ws >>. jvalue)

let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JObject)

jvalueRef.Value <-
    choice [ jobject
             jlist
             jstring
             jnumber
             jtrue
             jfalse
             jnull ]

let json = ws >>. jvalue .>> ws .>> eof

test
    json
    """[{"Case": "Node","Fields": ["A",[{"Case": "Node","Fields": ["B",[]]},{"Case": "Node","Fields": ["C",[]]}]]}]"""