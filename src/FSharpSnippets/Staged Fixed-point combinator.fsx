
// Staged Fixed-point combinator

open Microsoft.FSharp.Quotations

// <@ fun x -> (% <@ x @> ) @> ~ lambda (fun x -> x)
let lambda (f : Expr<'T> -> Expr<'R>) : Expr<'T -> 'R> =
    let var = new Var("__temp__", typeof<'T>)
    Expr.Cast<_>(Expr.Lambda(var,  f (Expr.Cast<_>(Expr.Var var))))


// fixed-point combinator
let rec fix : (('Τ -> 'R) -> ('Τ -> 'R)) -> 'Τ -> 'R = fun f x ->
    f (fix f) x

let power x f = 
    fun n -> 
        match n with 
        | 0 -> <@ 1 @> 
        | n -> <@ %x * (% f (n - 1) ) @> 

let power2 = fix (power <@ 2 @>)
power2 10 // loop unroll 10 times

// Staged fixed-point combinator
let fix' : (Expr<'T -> 'R> -> Expr<'T -> 'R>) -> Expr<'T -> 'R> = fun f ->
    <@ fun x -> let rec loop x = (% lambda (fun f' -> f f') ) loop x in loop x @>

let power' x f =
        <@ fun n ->
            match n with 
            | 0 -> 1  
            | n -> %x * (%f) (n - 1) @>

let power2' = fix' (power' <@ 2 @>)