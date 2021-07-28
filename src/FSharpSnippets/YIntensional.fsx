
// Inspired by https://arxiv.org/pdf/1703.01288.pdf

#r "nuget: FSharp.Quotations.Evaluator"

open FSharp.Linq.RuntimeHelpers
open FSharp.Quotations

let eval<'T> : Expr<'T> -> 'T = fun q -> 
    LeafExpressionConverter.EvaluateQuotation q :?> 'T

let eval'<'T> : Expr -> 'T = fun q -> 
    LeafExpressionConverter.EvaluateQuotation q :?> 'T


let fact : Expr<Expr<int -> int> -> int -> int> = 
    <@ fun f n -> if n = 0 then 1 else n * eval f (n - 1) @>


let Y : Expr<Expr<'T -> 'R> -> 'T -> 'R> -> Expr<'T -> 'R> = fun g ->
    let f : Expr<Expr -> 'T -> 'R> = 
        <@ fun f -> (%g) <@ fun x -> eval'<Expr -> 'T -> 'R> f f x @> @>
    <@ fun x -> eval f (f :> Expr) x @>


eval (Y fact) 10 