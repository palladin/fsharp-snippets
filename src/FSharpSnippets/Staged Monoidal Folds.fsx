// Staged Monoidal Folds based on https://github.com/Gabriel439/slides/blob/master/munihac/foldmap.md

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


// <@ fun x y -> (% <@ x @> ... <@ y @> ) @> ~ lambda (fun x y -> x ... y )
let lambda2 (f : Expr<'T> -> Expr<'S> -> Expr<'R>) : Expr<'T -> 'S -> 'R> =
    let [var; var'] = generateVars [typeof<'T>; typeof<'S>]
    Expr.Cast<_>(Expr.Lambda(var, Expr.Lambda(var',  f (Expr.Cast<_>(Expr.Var var)) (Expr.Cast<_>(Expr.Var var')))))


// data Fold i o = forall m . Monoid m => Fold (i -> m) (m -> o)
type Fold<'I, 'O> = 
    abstract member Invoke<'R> : FoldUnPack<'I, 'O, 'R> -> Expr<'R>

and FoldUnPack<'I, 'O, 'R> = 
    abstract member Invoke<'M> : Expr<'M> -> 
                                 (Expr<'M> -> Expr<'M> -> Expr<'M>) ->  
                                 (Expr<'I> -> Expr<'M>) ->
                                 (Expr<'M> -> Expr<'O>) -> Expr<'R>

and FoldCons<'M, 'I, 'O>(zero : Expr<'M>, plus : Expr<'M> -> Expr<'M> -> Expr<'M>, 
                         input : Expr<'I> -> Expr<'M>, output : Expr<'M> -> Expr<'O>) =
    interface Fold<'I, 'O> with
        member self.Invoke<'R> (unPack : FoldUnPack<'I, 'O, 'R>) : Expr<'R> = 
            unPack.Invoke<'M> zero plus input output


// combinators
let fold : Fold<'I, 'O> -> Expr<'I []> -> Expr<'O> = 
    fun mfold source ->
        mfold.Invoke<'O> 
            { new FoldUnPack<'I, 'O, 'O> with
                member self.Invoke<'M> (zero : Expr<'M>) plus input output =                      
                    <@  let mutable acc = %zero
                        for i = 0 to (%source).Length - 1 do
                            let current = (%source).[i]
                            let value = (% lambda (fun v -> input v)) current
                            acc <- (% lambda2 (fun acc v -> plus acc v)) acc value
                            ()
                        (% lambda (fun v -> output v)) acc @> }

let compile (f : Expr<'T> -> Expr<'R>) : 'T -> 'R = QuotationCompiler.ToFunc(lambda f) ()

// Examples
let sum : Fold<int, int> = 
    new FoldCons<int, int, int>(<@ 0 @>, (fun x y -> <@ %x + %y @>), id, id) :> _

let all : (Expr<'I> -> Expr<bool>) -> Fold<'I, bool> = fun p ->
    new FoldCons<bool, 'I, bool>(<@ true @>, (fun x y -> <@ %x && %y @>), (fun v -> p v), id) :> _

let any : (Expr<'I> -> Expr<bool>) -> Fold<'I, bool> = fun p ->
    new FoldCons<bool, 'I, bool>(<@ false @>, (fun x y -> <@ %x || %y @>), (fun v -> p v), id) :> _

let sumf = compile (fold sum)
sumf [|1; 2; 3|] // 6

let allf = compile (fold (all (fun v -> <@ %v % 2 = 0 @>)))
allf [|2; 4|] // true
allf [|1; 2; 4|] // false