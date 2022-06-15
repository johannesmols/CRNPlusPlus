open CRN.Core

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
    conc[a, 3],,, , ,conc[b, 0.4],, ,,
    step[ {
        sub[a, b, a], cmp[ 3sdf, ldöf],
        ,,
        div[la, sd, kljdsf]
    }],
    conc[d, 1337],
    step[{
        add[a, b, c],
        add[c, b, a],
        ifEQ[{ add[c, b, a] }]
    }]
};
"""
    
let res = parse input

match res with
| Result.Ok res -> printfn $"Success: {res}"
| Result.Error err -> printfn $"Failure: {err}"