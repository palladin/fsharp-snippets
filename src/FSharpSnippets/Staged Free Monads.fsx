// Staged Free Μonads

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


// Basic Types
type Ops<'Next> =
    | Set of Expr<int> * 'Next
    | Get of (Expr<int> -> 'Next)

type Program<'T> = 
    | Done of Expr<'T>
    | Wrap of Ops<Program<'T>>

// helper functions
let get : Program<int> = 
    Wrap <| Get (fun v -> Done v)

let set : Expr<int> -> Program<unit> = fun v ->
    Wrap <| Set (v, Done <@ () @>)

let map : ('T -> 'R) -> Ops<'T> -> Ops<'R> = 
    fun f p ->
        match p with
        | Set (v, next) -> Set (v, f next)
        | Get next -> Get (f << next)
    
let return' : Expr<'T> -> Program<'T> = fun v -> 
    Done v

let rec bind : (Expr<'T> -> Program<'R>) -> Program<'T> -> Program<'R> = 
    fun f p -> 
        match p with
        | Wrap ops ->
            Wrap <| map (bind f) ops 
        | Done v -> f v

let rec compile : Program<'T> -> Expr<int> -> (Expr<int> -> Expr<'T> -> Expr<'T>) -> Expr<'T> = 
    fun p x k -> 
        match p with
        | Wrap ops ->
            match ops with
            | Set (v, next) ->
                <@ let v' = %v in (% lambda (fun v -> compile next v k)) v' @>
            | Get f ->
                compile (f x) x k
        | Done v -> 
            k x v
                 

// Program Builder
type Program() = 
    member self.Return v = 
        return' v
    member self.Bind(p, f) = 
        bind f p

let prg = new Program()

// example
let example : Program<int> = 
    prg {
        let! v = get
        let! _ = set <@ %v * 2 @>
        let! v' = get
        return <@ %v' + 1 @>
    }

let exec = QuotationCompiler.ToFunc (lambda (fun x -> compile example x (fun _ v -> v))) ()

exec 2 // 5