// Polyvariadic fixpoint combinator in F# (heavily inspired by Haskell)

// http://okmij.org/ftp/Computation/fixed-point-combinators.html

let force (value : Lazy<_>) = value.Force()
let fix f = let rec x = lazy (f x) in x

let fix' (fs : list<Lazy<list<'a -> 'b>> -> 'a -> 'b>) : Lazy<list<'a -> 'b>> = 
    fix (fun r -> fs |> List.map (fun f -> f r)) 

let fe l x = 
    let [e; o] = force l
    x = 0 || o (x-1)
let fo l x = 
    let [e; o] = force l
    x <> 0 && e (x-1)
let l = fix' [fe; fo]
let [e; o] = force l
e 42 // true
o 42 // false