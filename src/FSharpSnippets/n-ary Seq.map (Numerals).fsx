// A pattern for creating n-ary Seq.map functions, based on numerals.

// For more info: ftp://ftp.cs.au.dk/pub/BRICS/RS/01/10/BRICS-RS-01-10.pdf

let (<*>) fs xs = Seq.map2 (fun f x -> f x) fs xs
let succ n fs xs = n (fs <*> xs)
let map n f = n (Seq.initInfinite (fun _ -> f))

// Numerals
let ``1``<'a1, 'r> : seq<('a1 -> 'r)> -> seq<'a1> -> seq<'r> = 
    succ id
let ``2``<'a1, 'a2, 'r> : seq<('a1 -> 'a2 -> 'r)> -> seq<'a1> -> seq<'a2> -> seq<'r> = 
    succ ``1``
let ``3``<'a1, 'a2, 'a3, 'r> : seq<('a1 -> 'a2 -> 'a3 -> 'r)> -> seq<'a1> -> seq<'a2> -> seq<'a3> -> seq<'r> = 
    succ ``2``

// Examples
map ``1`` (fun x -> x + 1) [1; 2] // [2; 3]
map ``2`` (fun x y -> x + y) [1; 2] [1; 2] // [2; 4]
map ``3`` (fun x y z -> x + y + z) [1; 2] [1; 2] [1; 2] // [3; 6]