
// Quotations to A-Normal form
// based on http://matt.might.net/articles/a-normalization/

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
 

let rec normalize (expr : Expr) (k : Expr -> Expr) : Expr = 
    match expr with
    | Var var -> k expr
    | Value(value, _) -> k expr
    | Let (var, expr, body) -> 
        normalize expr (fun expr' -> Expr.Let(var, expr', normalize body k))
    | Lambda (var, body) -> 
        k <| Expr.Lambda (var, normalize body id)
    | Application (exprF, arg) -> 
        normalizeName exprF (fun exprF' -> normalizeName arg (fun arg' -> k <| Expr.Application(exprF', arg')))  
    | IfThenElse (pred, trueExpr, falseExpr) -> 
        normalizeName pred (fun pred' -> 
        k <| Expr.IfThenElse (pred', normalize trueExpr id, normalize falseExpr id))
    | _ -> failwithf "Not supported, expr: %A" expr

and normalizeName (expr : Expr) (k : Expr -> Expr) : Expr = 
    normalize expr (fun expr' -> 
        match expr' with
        | Var var -> k expr'
        | Value(value, _) -> k expr'
        | _ -> let var = new Var("temp", expr'.Type)
               Expr.Let(var, expr', k <| Expr.Var var))

// Examples
normalize <@ let x = let y = 1 in y in x @> id
normalize <@ if (let x = true in x) then 1 else 2 @> id
normalize <@ (fun f x -> f x) ((fun x -> x) (fun x -> x)) 1 @> id
