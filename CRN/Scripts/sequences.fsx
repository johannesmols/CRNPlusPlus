// "Brute forcing" graph values for multiplication via sequences:
// initial values: A = 6, B = 2, C = 0
// mul[A, B, C] produces the following derivative functions:
// A' = 0
// B' = 0
// C' = A * B - C(t)
//
// C starts at 0: C(0) = 0
// From there, we calculate the derivative at 0, and go just a tiny bit forward based on it
// Recalculate derivative at new value, go a bit more, etc.

//* example 1 (slow, fully recursive)

let b t = 2.0

let a t = 6.0

let rec c t prec =
    match t with
    | _ when t <= 0.0 -> 0.0
    | _ -> c (t - prec) prec + prec * c' (t - prec) prec

and c' t p = a t * b t - c t p

let rec s1 p =
    Seq.initInfinite (fun i -> c (p * float i) p)

// More than 27 can start to take a while
s1 0.01 |> Seq.take 27 |> Seq.toList

//* example 2 (fast, using previous computed values)

let B t = 2.0

let A t = 6.0

let C' t Ct = A t * B t - Ct

let C t prec prev =
    match t with
    | _ when t <= 0.0 -> 0.0
    | _ -> prev + prec * C' (t - prec) prev

let s2 prec =
    (0.0, 0.0)
    |> Seq.unfold (fun (t, prev) ->
        let result = C t prec prev

        Some(result, (t + prec, result)))

s2 0.001 |> Seq.take (100 * 1000) |> Seq.toList

// Calculated value: 11.99945792
// Actual value:     11.9994552; gotten from solving the equation: C(t) = 12 - 12*e^(-t)
Seq.item 100000 (s2 0.001)