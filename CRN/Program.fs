open CRN.Core

open FParsec

let input = """
crn = {
    conc[a, a0],
    conc[b, b0],
    step[{
        ld[a, atmp],
        ld[b, btmp],
        cmp[a, b]
    }],
    step[{
        ifGT[{ sub[atmp, btmp, a] }],
        ifLT[{ sub[btmp, atmp, b] }]
    }]
};
"""

let input2 = """
crn = {
    conc[a, 3],,, , ,conc[b, 4],, ,,
    step[ {
        sub[a, b, a], cmp[ 3sdf, ldöf],
        ,,
        div[la, sd, kljdsf]
    }  ]
};
"""

let parse input =
    match run programFull input with
    | Success(res, _, _) -> Result.Ok res
    | Failure(err, _, _) -> Result.Error err
    
let res = parse input

match res with
| Result.Ok res -> printfn $"Success: {res}"
| Result.Error err -> printfn $"Failure: {err}"