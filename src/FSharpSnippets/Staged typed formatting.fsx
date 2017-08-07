// Staged typed formatting, based on http://okmij.org/ftp/ML/GADT.txt, https://www.cl.cam.ac.uk/~jdy22/papers/modular-macros.pdf

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
    let sym : Eq<'A, 'B> -> Eq<'B, 'A> = fun (Refl (f, g)) -> Refl (g, f)
    let cast : Eq<'A, 'B> -> Expr<'A> -> Expr<'B> = fun (Refl (f, _)) -> f

open Eq

// Basic type
type Fmt<'A, 'B> =
  | FLit of Eq<'A, 'B> * string
  | FInt of Eq<int -> 'B, 'A>
  | FChar of Eq<char -> 'B, 'A>
  | FCmp of Compose<'A, 'B>

and Compose<'A, 'B> =
    abstract Invoke<'R> : Handler<'A, 'B, 'R> -> Expr<'R>
and Handler<'A, 'C, 'R> =
    abstract Handle<'B> : Fmt<'A, 'B> * Fmt<'B, 'C> -> Expr<'R>

// helper functions
let flit : string -> Fmt<'A, 'A> = 
    fun x -> FLit (refl (), x)

let fint : unit -> Fmt<int -> 'A, 'A> =
    fun () -> FInt (refl ())

let fchar : unit -> Fmt<char -> 'A, 'A> =
    fun () -> FChar (refl ())

let cmp : Fmt<'A, 'B> -> Fmt<'B, 'C> -> Fmt<'A, 'C> =
    fun left right -> 
        FCmp <|    
            { new Compose<'A, 'C> with
                member self.Invoke<'R>(handler : Handler<'A, 'C, 'R>) = 
                    handler.Handle<'B>(left, right) } 

let (%) a b = cmp a b

let example () = flit "(" % fchar() % flit "," % fint() % flit ")"


let rec printer<'A, 'B> : Fmt<'A, 'B> -> (Expr<string> -> Expr<'B>) -> Expr<'A> = 
    fun fmt k ->
        match fmt with
        | FLit (eq, x) -> cast (sym eq) (k <@ x @>)
        | FInt eq -> cast eq (<@ fun x -> (% lambda (fun x -> k <@ string %x @>)) x @>)
        | FChar eq -> cast eq (<@ fun x -> (% lambda (fun x -> k <@ string %x @>)) x @>)
        | FCmp cmp -> 
            cmp.Invoke<'A> {
                new Handler<'A, 'B, 'A> with
                    member self.Handle<'C>(left : Fmt<'A, 'C>, right : Fmt<'C, 'B>) : Expr<'A> = 
                        printer<'A, 'C> left (fun x -> printer<'C, 'B> right (fun y -> k <@ %x + %y @>))
            }
let exampleExpr = printer (example ()) id

let f = QuotationCompiler.ToFunc exampleExpr ()

f '2' 1 // (2,1)

