// Partial evaluating the Ackermann function.

// http://lambda-the-ultimate.org/node/4039#comment-61431

open Microsoft.FSharp.Quotations

// <@ fun x -> (% <@ x @> ) @> ~ lambda (fun x -> x)
let lambda (f : Expr<'T> -> Expr<'R>) : Expr<'T -> 'R> =
    let var = new Var("__temp__", typeof<'T>)
    Expr.Cast<_>(Expr.Lambda(var,  f (Expr.Cast<_>(Expr.Var var))))

// Staged fixed-point combinator
let fix : (Expr<'T -> 'R> -> Expr<'T -> 'R>) -> Expr<'T -> 'R> = fun f ->
    <@ fun x -> let rec loop x = (% lambda (fun f' -> f f') ) loop x in loop x @>

let rec ack (m : int) : Expr<int -> int> = 
    fix (fun f -> lambda (fun (n : Expr<int>) ->
        if m = 0 then <@ %n + 1 @>
        else <@ if %n = 0 then (% ack (m - 1)) 1
                else (% ack (m - 1)) ((%f) (%n - 1)) @>)) 

// Example 
let ack2 = ack 2 // m=2