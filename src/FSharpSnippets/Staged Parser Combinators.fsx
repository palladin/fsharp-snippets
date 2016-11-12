// Staged parser combinators

#r "packages/FSharp.Compiler.Service.1.3.1.0/lib/net45/FSharp.Compiler.Service.dll"
#r "packages/QuotationCompiler.0.0.7-alpha/lib/net45/QuotationCompiler.dll"


open QuotationCompiler

open Microsoft.FSharp.Quotations

let counter = ref 0

// <@ fun x -> (% <@ x @> ) @> ~ lambda (fun x -> x)
let lambda (f : Expr<'T> -> Expr<'R>) : Expr<'T -> 'R> =
    incr counter
    let var = new Var(sprintf "__paramTemp_%d__" !counter, typeof<'T>)
    Expr.Cast<_>(Expr.Lambda(var,  f (Expr.Cast<_>(Expr.Var var))))


// <@ fun x y -> (% <@ x @> ... <@ y @> ) @> ~ lambda (fun x y -> x ... y )
let lambda2 (f : Expr<'T> -> Expr<'S> -> Expr<'R>) : Expr<'T -> 'S -> 'R> =
    incr counter
    let var = new Var(sprintf "__paramTemp_%d__" !counter, typeof<'T>)
    incr counter
    let var' = new Var(sprintf "__paramTemp_%d__" !counter, typeof<'S>)
    Expr.Cast<_>(Expr.Lambda(var, Expr.Lambda(var',  f (Expr.Cast<_>(Expr.Var var)) (Expr.Cast<_>(Expr.Var var')))))

type Parser<'T> = Expr<string> -> Expr<int> -> (Expr<'T> -> Expr<string> -> Expr<int> -> Expr<bool>) -> Expr<bool>


// combinators
let pchar (matchChar : char) : Parser<char> = 
    fun str index k ->
        <@ let index = %index
           if (%str).Length = index then false 
           else 
            let current = (%str).[index]
            if current <> matchChar then 
                false
            else
                (% lambda2 (fun current index -> k current str index)) current (index + 1) @>


let (=>) (left : Parser<'T>) (right : Parser<'S>) : Parser<'T * 'S> = 
    fun str index k -> failwith "oups"

let (<|>) (left : Parser<'T>) (right : Parser<'T>) : Parser<'T> = 
    fun str index k -> failwith "oups"

let (<*>) (parser : Parser<'T>) : Parser<'T []> = 
    fun str index k -> failwith "oups"

let compileParser (parser : Parser<'T>) : string -> 'T option = 
    let f = QuotationCompiler.ToFunc(lambda (fun (str : Expr<string>) -> 
                                        <@ let resultRef = ref Unchecked.defaultof<'T>
                                           let test = (% lambda (fun resultRef -> parser str <@ 0 @>  (fun value _ _ -> <@ %resultRef := %value; true @>))) resultRef
                                           if test then Some !resultRef else None
                                        @>))
    (fun str -> f () str)

    

// Examples
let f = pchar 'a' |> compileParser
f "abc" // Some 'a'
f "bac" // None





