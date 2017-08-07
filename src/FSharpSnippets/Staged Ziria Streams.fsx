// Staged Ziria Streams, based on https://github.com/dimitriv/ziria-sem/blob/master/Haskell/ZirBasic.hs

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

// Basic type
type Zir<'I, 'O, 'V> =
    | Yield of Zir<'I, 'O, 'V> * Expr<'O>
    | Done of Expr<'V>
    | NeedInput of (Expr<'I> -> Zir<'I, 'O, 'V>)

// helper functions
let emit : Expr<'O> -> Zir<'I, 'O, unit> = 
    fun o -> Yield (Done <@ () @>, o)

let take : Zir<'I, 'O, 'I> = 
    NeedInput Done

let return' : Expr<'V> -> Zir<'I, 'O, 'V> =
    Done 

let rec bind : Zir<'I, 'O, 'V> -> (Expr<'V> -> Zir<'I, 'O, 'W>) -> Zir<'I, 'O, 'W> =
    fun z f ->
        match z with
        | Done v -> f v
        | Yield (z', o) -> Yield (bind z' f, o)
        | NeedInput g -> NeedInput (fun i -> bind (g i) f)

let (>>>) : Zir<'I, 'M, 'V> -> Zir<'M, 'O, 'V> -> Zir<'I, 'O, 'V> =
    fun z1 z2 ->
        let rec go2 : Zir<'I, 'M, 'V> -> Zir<'M, 'O, 'V> -> Zir<'I, 'O, 'V> =
            fun z1 z2 -> 
                match z2 with
                | Done v -> Done v
                | Yield (z2', o) -> Yield (go2 z1 z2', o)
                | NeedInput g -> go1 g z1
        and go1 : (Expr<'M> -> Zir<'M, 'O, 'V>) -> Zir<'I, 'M, 'V> -> Zir<'I, 'O, 'V> =
            fun g z -> 
                match z with
                | Done v -> Done v
                | Yield (z1', o) -> go2 z1' (g o)
                | NeedInput g' -> NeedInput (go1 g << g')
        go2 z1 z2

// Builder type
type ZirBuilder() =
    member self.Return (v : Expr<'V>) = return' v
    member self.Bind(z : Zir<'I, 'O, 'V>, f : Expr<'V> -> Zir<'I, 'O, 'W>) =
        bind z f

let zir = new ZirBuilder()

// example
let example1 : Zir<int, string, unit> = 
    zir {
        let! x = take
        let! _ =  emit <@ string (%x + 1) @>
        let! _ =  emit <@ string (%x + 2) @>
        return <@ () @>
    }

let example2 : Zir<string, int, unit> = 
    zir {
        let! x = take
        let! y = take
        let! _ = emit <@ (System.Int32.Parse %x) + (System.Int32.Parse %y) @>
        return <@ () @>
    }
let example : Zir<int, int, unit> = example1 >>> example2


let rec run : Expr<'I> -> (Expr<'O> -> Expr<unit>) -> Zir<'I, 'O, 'V> -> Expr<'V> =
    fun input output z ->
        match z with
        | Done v -> v
        | Yield (z', o) -> <@ let o' = %o in (% lambda (fun o' -> output o')) o'; (% run input output z') @>
        | NeedInput f -> <@ let i = %input in (% lambda(fun i -> run input output (f i))) i @>

let example' =
    <@ for i = 1 to 10 do
         (% lambda (fun i -> run i (fun o -> <@ printfn "%d" %o @>) example)) i
    @>

let _ = QuotationCompiler.ToFunc example' ()

