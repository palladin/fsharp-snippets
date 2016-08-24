// Encoding mutually-recursive functions with a Polyvariadic fixpoint combinator.

let rec Y f x = f (Y f) x 

let rec Y2 f1 f2 =
    let f1' = Y (fun f1' -> f1 f1' (Y (fun f2' -> f2 f1' f2')))
    let f2' = Y (fun f2' -> f2 (Y (fun f1' -> f1 f1' f2')) f2')
    f1', f2'

// Example
let even, odd = 
    Y2 (fun even odd x ->
           x = 0 || odd (x-1))
       (fun even odd x ->
           x <> 0 && even (x-1))

even 42 // true 
odd 42 // false