// Staged Trampoline
#r "packages/FSharp.Compiler.Service.1.3.1.0/lib/net45/FSharp.Compiler.Service.dll"
#r "packages/QuotationCompiler.0.0.7-alpha/lib/net45/QuotationCompiler.dll"
#r "bin/FSharpSnippets.dll"

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


module Cont = 
    // Basic type
    type Cont<'T, 'R> = (((Expr<'T> -> Expr<unit>) -> Expr<int> -> (Expr<int> -> Expr<unit>) -> Expr<'R>) -> int -> Expr<'R>) -> int -> Expr<'R>

    // helper functions
    let return' : Expr<'T> -> Cont<'T, 'R> =
        fun v k spc -> k (fun f pc upc -> 
                                <@ if %pc = spc then 
                                    (% f v)
                                   Unchecked.defaultof<'R> @>) (spc + 1)
    let do' = return'

    let bind : Cont<'T, 'R> -> (Expr<'T> -> Cont<'V, 'R>) -> Cont<'V, 'R> = 
        fun cont f k spc -> 
            <@ 
                let v = ref Unchecked.defaultof<'T>
                (% lambda (fun v -> cont (fun k' -> 
                                            f <@ !(%v) @> (fun k'' -> 
                                                            k (fun u pc upc -> 
                                                                    <@ (% k' (fun v' -> <@ %v := %v' @>) pc upc) |> ignore; 
                                                                       (% k'' u pc upc) @>))) spc)) v
            @>

    let setjmp : Cont<int, 'R> =
        fun k spc -> k (fun f pc upc -> 
                            <@ if %pc = spc then 
                                (% f pc)
                               Unchecked.defaultof<'R> @>) (spc + 1)

    let longjmp : Expr<int> -> Cont<unit, 'R> =
        fun jmp k spc -> k (fun f pc upc -> 
                                <@ if %pc = spc then 
                                    (% upc <@ %jmp - 1 @>)
                                    (% f <@ () @>)
                                   Unchecked.defaultof<'R> @>) (spc + 1)


    type ContBuilder() = 
        member self.Return v = return' v
        member self.Bind(cont, f) = bind cont f

    let cont = new ContBuilder()

    let compile : Cont<'T, 'T> -> Expr<'T> = 
        fun cont ->
            cont (fun k spc -> <@ let r = ref Unchecked.defaultof<'T>
                                  let pc = ref 0
                                  while !pc >= 0 && !pc < spc do
                                    (% lambda2 (fun r pc -> k (fun i -> <@ %r := %i @>) <@ !(%pc) @> (fun pc' -> <@ %pc := %pc' @>))) r pc |> ignore;
                                    incr pc
                                  !r @>) 0 

open Cont

// Example
let example () =
    cont {
        let! jmp = setjmp
        let! x = cont { return <@ 1 @> }
        let! _ = do' <@ printfn "x: %d" %x @>
        let! y = cont { return <@ 2 @> }
        let! _ = do' <@ printfn "y: %d" %y @>
        let! _ = longjmp jmp
        return <@ () @>
    }

let f = QuotationCompiler.ToFunc <| compile (example ())
f()



