
// Normalisation of "F# quotations" by evaluation.

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns


// Helper active patters for System.Type
let (|Arrow|_|) (t : Type) = 
    if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ -> _> then
        let args = t.GetGenericArguments()
        Some (args.[0], args.[1])
    else
        None

let (|Base|_|) (t : Type) =
    if t = typeof<obj> then
        Some t
    else
        None


// Semantic Domain
type Sem = 
    | Lam of (Sem -> Sem)
    | Expr of Expr

// reflect : Expr -> Sem
let rec reflect (expr : Expr) = 
    match expr.Type with
    | Arrow (a, b) -> 
        Lam (fun sem -> reflect <| Expr.Application(expr, reify a sem)) 
    | Base _ -> Expr expr 
    | t -> failwithf "Not supported, type: %s" t.Name

// reify : Type -> Sem -> Expr
and reify (t : Type) (sem : Sem) = 
    match t, sem with
    | Arrow (a, b), Lam f -> 
        let var = new Var("var", a) // fresh var
        Expr.Lambda(var, reify b (f (reflect (Expr.Var(var)))))
    | Base _, Expr expr -> expr
    | _ -> failwith "Invalid state"

// meaning : Map<Var, Sem> -> Expr -> Sem
let rec meaning (ctx : Map<Var, Sem>) (expr : Expr) = 
    match expr with
    | Var var -> ctx.[var]
    | Lambda (var, body) -> 
        Lam (fun sem -> meaning (Map.add var sem ctx) body)
    | Application (f, s) -> 
        match meaning ctx f with
        | Lam f' -> f' (meaning ctx s)
        | _ -> failwith "Invalid state"
    | _ -> failwithf "Not supported, expr: %A" expr

// nbe : Expr -> Expr
let nbe (expr : Expr) = 
    reify expr.Type (meaning Map.empty expr)

// Example
let K () = <@ fun x y -> x @>
let S () = <@ fun x y z -> x z (y z) @>
let SKK () = <@ (%S ()) (%K ()) (%K ()) @>

nbe <| SKK () // Lambda (var, var)