// Fusion via reducer partial application
// based on http://manojo.github.io/resources/staged-fold-fusion.pdf 

#r "packages/FSharp.Compiler.Service.1.3.1.0/lib/net45/FSharp.Compiler.Service.dll"
#r "packages/QuotationCompiler.0.0.7-alpha/lib/net45/QuotationCompiler.dll"

open QuotationCompiler
open Microsoft.FSharp.Quotations

// <@ fun x -> (% <@ x @> ) @> ~ lambda (fun x -> x)
let lambda (f : Expr<'T> -> Expr<'R>) : Expr<'T -> 'R> =
    let var = new Var("__temp__", typeof<'T>)
    Expr.Cast<_>(Expr.Lambda(var,  f (Expr.Cast<_>(Expr.Var var))))

// <@ fun x y -> (% <@ x @> ... <@ y @> ) @> ~ lambda (fun x y -> x ... y )
let lambda2 (f : Expr<'T> -> Expr<'S> -> Expr<'R>) : Expr<'T -> 'S -> 'R> =
    let var = new Var("__temp__", typeof<'T>)
    let var' = new Var("__temp'__", typeof<'S>)
    Expr.Cast<_>(Expr.Lambda(var, Expr.Lambda(var',  f (Expr.Cast<_>(Expr.Var var)) (Expr.Cast<_>(Expr.Var var')))))


   
// Basic Structure
type Reducer<'T> = 
    abstract Apply<'R> : (Expr<'R> -> Expr<'T> -> Expr<'R>) -> Expr<'R> -> Expr<'R> 
        
// Basic operations
let ofArray (array : Expr<'T []>) : Reducer<'T> = 
    { new Reducer<'T> with
        member self.Apply<'R> (f : Expr<'R> -> Expr<'T> -> Expr<'R>) (s : Expr<'R>) =
            <@  let mutable i = 0
                let mutable s = %s
                while i < (%array).Length do
                    let v = (%array).[i]
                    s <- (%lambda2 f) s v
                    i <- i + 1
                s @> }
    

let map (f : Expr<'A> -> Expr<'B>) (reducer : Reducer<'A>) : Reducer<'B> = 
    { new Reducer<'B> with 
        member self.Apply<'R> (f' : Expr<'R> -> Expr<'B> -> Expr<'R>) (s : Expr<'R>) : Expr<'R> = 
                reducer.Apply<'R> (fun s' a -> f' s' (f a)) s }

let filter (p : Expr<'T> -> Expr<bool>) (reducer : Reducer<'T>) : Reducer<'T> = 
    { new Reducer<'T> with 
        member self.Apply<'R> (f : Expr<'R> -> Expr<'T> -> Expr<'R>) (s : Expr<'R>) : Expr<'R> = 
                reducer.Apply<'R> (fun s' a -> <@ if (% p a) then (% f s' a) else %s' @>) s }

let fold (f : Expr<'S> -> Expr<'T> -> Expr<'S>) (s : Expr<'S>) (reducer : Reducer<'T>) : Expr<'S> = 
    reducer.Apply<'S> f s 

let compile (f : Expr<'T -> 'R>) : 'T -> 'R = QuotationCompiler.ToFunc(f) ()


// Example

let test arr = 
    arr 
    |> ofArray
    |> filter (fun v -> <@ %v % 2 = 0 @>)
    |> map (fun v -> <@ %v * 2 @>)
    |> fold (fun s v -> <@ %s + %v @>) <@ 0 @>

let f = test |> lambda |> compile
f [|1..100|] // 5100
