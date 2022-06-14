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
    conc[a, a0],,, , ,conc[b, b0],, ,,
    step[ {
        ifGT[ { sub[a, b, a] } ],
        ifGT[ { sub[a, b, a] } ],
    }  ]
};
"""

let parse input =
    match run programFull input with
    | Success(res, _, _) -> Result.Ok res
    | Failure(err, _, _) -> Result.Error err
    
let res = parse input2

match res with
| Result.Ok res -> printfn $"Success: {res}"
| Result.Error err -> printfn $"Failure: {err}"