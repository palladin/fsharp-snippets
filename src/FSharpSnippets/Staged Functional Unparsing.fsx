// Staged Functional Unparsing based on http://www.brics.dk/RS/98/12/BRICS-RS-98-12.pdf

#r "packages/FSharp.Compiler.Service.1.3.1.0/lib/net45/FSharp.Compiler.Service.dll"
#r "packages/QuotationCompiler.0.0.7-alpha/lib/net45/QuotationCompiler.dll"

open System
open QuotationCompiler
open Microsoft.FSharp.Quotations

let counter = ref 0

let rec generateVars (types : Type list) : Var list = 
    match types with 
    | [] -> []
    | t :: ts -> 
        incr counter
        let var = new Var(sprintf "__paramTemp_%d__" !counter, t)
        var :: generateVars ts

// <@ fun x -> (% <@ x @> ) @> ~ lambda (fun x -> x)
let lambda (f : Expr<'T> -> Expr<'R>) : Expr<'T -> 'R> =
    let [var] = generateVars [typeof<'T>]
    Expr.Cast<_>(Expr.Lambda(var,  f (Expr.Cast<_>(Expr.Var var))))


// <@ fun x y -> (% <@ x @> ... <@ y @> ) @> ~ lambda (fun x y -> x ... y )
let lambda2 (f : Expr<'T> -> Expr<'S> -> Expr<'R>) : Expr<'T -> 'S -> 'R> =
    let [var; var'] = generateVars [typeof<'T>; typeof<'S>]
    Expr.Cast<_>(Expr.Lambda(var, Expr.Lambda(var',  f (Expr.Cast<_>(Expr.Var var)) (Expr.Cast<_>(Expr.Var var')))))

// <@ fun x y z -> (% <@ x @> ... <@ y @> ... <@ z @> ) @> ~ lambda (fun x y z -> x ... y ... z )
let lambda3 (f : Expr<'T> -> Expr<'S> -> Expr<'K> -> Expr<'R>) : Expr<'T -> 'S -> 'K -> 'R> =
    let [var; var'; var''] = generateVars [typeof<'T>; typeof<'S>; typeof<'K>]
    Expr.Cast<_>(Expr.Lambda(var, Expr.Lambda(var', Expr.Lambda(var'', f (Expr.Cast<_>(Expr.Var var)) (Expr.Cast<_>(Expr.Var var')) (Expr.Cast<_>(Expr.Var var''))))))



// combinators
let lit : Expr<string> -> (Expr<string> -> 'T) -> Expr<string> -> 'T = 
    fun x k s -> k <@ %s + %x @>

let eol : (Expr<string> -> 'T) -> Expr<string> -> 'T = 
    fun k s -> k <@ %s + Environment.NewLine @>

let int : (Expr<string> -> 'T) -> Expr<string> -> Expr<int> -> 'T = 
    fun k s x -> k <@ %s + string %x @>

let str : (Expr<string> -> 'T) -> Expr<string> -> Expr<string> -> 'T = 
    fun k s x -> k <@ %s + %x @>

let format : ((Expr<string> -> Expr<string>) -> Expr<string> -> 'T) -> 'T = 
    fun p ->  p id <@ "" @>

let compile (f : Expr<'T> -> Expr<'R>) : 'T -> 'R = QuotationCompiler.ToFunc(lambda f) ()
let compile2 (f : Expr<'T> -> Expr<'S> -> Expr<'R>) : 'T -> 'S -> 'R = QuotationCompiler.ToFunc(lambda2 f) ()
let compile3 (f : Expr<'T> -> Expr<'S> -> Expr<'K> -> Expr<'R>) : 'T -> 'S -> 'K -> 'R = QuotationCompiler.ToFunc(lambda3 f) ()

// Examples
let f  = compile2 <| format (int << lit <@ " is " @> << str)
f 42 "number" // "42 is number"
let g = compile2 <| format (int << lit <@ "/" @> << int)
g 1 2 // "1/2"

