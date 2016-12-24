// Lightweight Staged Numeric code

#r "packages/FSharp.Compiler.Service.1.3.1.0/lib/net45/FSharp.Compiler.Service.dll"
#r "packages/QuotationCompiler.0.0.7-alpha/lib/net45/QuotationCompiler.dll"

open System
open QuotationCompiler
open Microsoft.FSharp.Quotations

// <@ fun x -> (% <@ x @> ) @> ~ lambda (fun x -> x)
let lambda (f : Expr<'T> -> Expr<'R>) : Expr<'T -> 'R> =
    let var = new Var("__temp__", typeof<'T>)
    Expr.Cast<_>(Expr.Lambda(var,  f (Expr.Cast<_>(Expr.Var var))))


type ExprPlus = ExprPlus with
    static member inline (?<-) (a, ExprPlus, b) = <@ %a + %b @>
    static member inline (?<-) (a, ExprPlus, b) = a + b

let inline (+) a b =  a ? (ExprPlus) <- b

type ExprTimes = ExprTimes with
    static member inline (?<-) (a : Expr<_>, ExprTimes, b : Expr<_>) = <@ %a * %b @>
    static member inline (?<-) (a, ExprTimes, b) = a * b

let inline (*) a b =  a ? (ExprTimes) <- b

module NumericLiteralG = 
    let inline FromZero() = let zero = LanguagePrimitives.GenericZero in <@ zero @>
    let inline FromOne() = let one = LanguagePrimitives.GenericOne in <@ one @>
    let inline FromInt32 (n : int) = <@ n @>

let compile (f : Expr<'T> -> Expr<'R>) : 'T -> 'R = QuotationCompiler.ToFunc(lambda f) ()

// Example
let f : Expr<int> -> Expr<int> = 
    (fun x -> x * 2G) << (fun x -> x + 1G)

compile f 2 // 6
 
