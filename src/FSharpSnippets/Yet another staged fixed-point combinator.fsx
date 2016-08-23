
// Staged fixed-point combinator without staged recursion.

open Microsoft.FSharp.Quotations

// <@ fun x -> (% <@ x @> ) @> ~ lambda (fun x -> x)
let lambda (f : Expr<'T> -> Expr<'R>) : Expr<'T -> 'R> =
    let var = new Var("__temp__", typeof<'T>)
    Expr.Cast<_>(Expr.Lambda(var,  f (Expr.Cast<_>(Expr.Var var))))



// Staged fixed-point combinator
let fix : (Expr<'T -> 'R> -> Expr<'T -> 'R>) -> Expr<'T -> 'R> = fun f ->
    <@  let r = ref Unchecked.defaultof<_>
        r := (% lambda (fun r -> f <@ !(%r) @> ) ) r
        !r @>
         
    
let power x f =
        <@ fun n ->
            match n with 
            | 0 -> 1  
            | n -> %x * (%f) (n - 1) @>

let power2 = fix (power <@ 2 @>)