// Staged Higher-order abstract syntax via GADT encoding

#r "packages/FSharp.Compiler.Service.1.3.1.0/lib/net45/FSharp.Compiler.Service.dll"
#r "packages/QuotationCompiler.0.0.7-alpha/lib/net45/QuotationCompiler.dll"

open System
open QuotationCompiler
open Microsoft.FSharp.Quotations

// helper functions
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

// Helper Equality type
module Eq = 
    type Eq<'A, 'B> = private Refl of (Expr<'A> -> Expr<'B>) * (Expr<'B> -> Expr<'A>)
    
    let refl<'A> () : Eq<'A, 'A> = Refl (id, id)
    let cast : Eq<'A, 'B> -> Expr<'A> -> Expr<'B> = fun (Refl (f, _)) -> f

open Eq

// HOAS for a simple language
type Exp<'T> = 
    abstract member Invoke : Lambda<'T> -> Expr<'T>
and Lambda<'T> = 
    abstract member Invoke<'S> : Eq<'S, 'T> * Expr<'S> -> Expr<'T>
    abstract member Invoke<'S, 'K> : Eq<'S * 'K, 'T> * Exp<'S> * Exp<'K> -> Expr<'T>
    abstract member Invoke<'S, 'K> : Eq<'S -> 'K, 'T> * (Exp<'S> -> Exp<'K>) -> Expr<'T>
    abstract member Invoke<'S, 'K> : Eq<'K, 'T> * (Exp<'S -> 'K>) * Exp<'S> -> Expr<'T>
    abstract member Invoke<'S, 'K> : Eq<'S -> 'K, 'T> * (Exp<('S -> 'K) -> ('S -> 'K)>) -> Expr<'T>


type Value<'T>(value : Expr<'T>) =
    interface Exp<'T> with
        member self.Invoke lambda = lambda.Invoke(refl (), value) 

type Tup<'T, 'S>(first : Exp<'T>, second : Exp<'S>) =
    interface Exp<'T * 'S> with
        member self.Invoke lambda = lambda.Invoke(refl (), first, second)

type Lam<'T, 'S>(f : Exp<'T> -> Exp<'S>) =
    interface Exp<'T -> 'S> with
        member self.Invoke lambda = lambda.Invoke(refl (), f)

type App<'T, 'S>(f : Exp<'T -> 'S>, x : Exp<'T>) =
    interface Exp<'S> with
        member self.Invoke lambda = lambda.Invoke(refl (), f, x)

type Rec<'T, 'S>(f : Exp<('T -> 'S) -> ('T -> 'S)>) =
    interface Exp<'T -> 'S> with
        member self.Invoke lambda = lambda.Invoke(refl (), f)
    
// helper combinators
let lift (value : Expr<'T>) : Exp<'T> = 
    new Value<'T>(value) :> _

let tup (first : Exp<'T>) (second : Exp<'S>) : Exp<'T * 'S>  = 
    new Tup<'T, 'S>(first, second) :> _

let lam (f : Exp<'T> -> Exp<'S>) : Exp<'T -> 'S> = 
    new Lam<'T, 'S>(f) :> _

let app (f : Exp<'T -> 'S>) (x : Exp<'T>) : Exp<'S> = 
    new App<'T, 'S>(f, x) :> _

let rec fix (f : Exp<('T -> 'S) -> ('T -> 'S)>) : Exp<'T -> 'S> =
    new Rec<'T, 'S>(f) :> _

// compile HOAS to quotation
let rec compile<'T> (exp : Exp<'T>) : Expr<'T> = 
    exp.Invoke {
        new Lambda<'T> with
            member self.Invoke<'S>(eq : Eq<'S, 'T>, v : Expr<'S>) : Expr<'T> = 
                cast eq v
            member self.Invoke<'S, 'K>(eq : Eq<'S * 'K, 'T>, first : Exp<'S>, second : Exp<'K>) : Expr<'T> = 
                cast eq <@ ((% compile first) , (% compile second)) @>
            member self.Invoke<'S, 'K>(eq : Eq<'S -> 'K, 'T>, f : Exp<'S> -> Exp<'K>) : Expr<'T> = 
                let f' = lambda (fun v -> (compile (f (lift v)))) 
                cast eq f'
            member self.Invoke<'S, 'K>(eq : Eq<'K, 'T>, f : Exp<'S -> 'K>, v : Exp<'S>) : Expr<'T> =
                let r = <@ (% compile f) (% compile v) @>
                cast eq r
            member self.Invoke<'S, 'K>(eq : Eq<'S -> 'K, 'T>, f : Exp<('S -> 'K) -> ('S -> 'K)>) : Expr<'T> = 
                let f' = <@ fun x ->
                             let rec loop x = 
                                (% compile f) loop x 
                             loop x @>
                cast eq f'
    }
// Example
let fact = compile 
              (fix (lam (fun f -> 
                    lam (fun y -> 
                        let y' = compile y
                        lift (<@ if %y' = 0 then 1 else %y' * (% compile f) (%y' - 1) @>)))))

let fact' = QuotationCompiler.ToFunc fact ()

fact' 4