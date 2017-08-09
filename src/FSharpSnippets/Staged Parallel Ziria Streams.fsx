// Staged Parallel Ziria Streams

#r "packages/FSharp.Compiler.Service.1.3.1.0/lib/net45/FSharp.Compiler.Service.dll"
#r "packages/QuotationCompiler.0.0.7-alpha/lib/net45/QuotationCompiler.dll"
#r "bin/FSharpSnippets.dll"

open System
open QuotationCompiler
open Microsoft.FSharp.Quotations
open System.Threading
open System.Threading.Tasks
open System.Collections.Concurrent


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

// Helper Equality type
module Eq = 
    type Eq<'A, 'B> = private Refl of (Expr<'A> -> Expr<'B>) * (Expr<'B> -> Expr<'A>)
    
    let refl<'A> () : Eq<'A, 'A> = Refl (id, id)
    let cast : Eq<'A, 'B> -> Expr<'A> -> Expr<'B> = fun (Refl (f, _)) -> f

open Eq

// Basic types
type Zir<'I, 'O, 'V> =
    abstract member Invoke : Handler<'I, 'O, 'V> -> Expr<'V>
and Handler<'I, 'O, 'V> = 
    abstract member Handle : Eq<unit, 'V> * Expr<'O> -> Expr<'V>
    abstract member Handle : Expr<'V> -> Expr<'V>
    abstract member Handle : Eq<'I, 'V> -> Expr<'V>
    abstract member Handle : Eq<bool, 'V> -> Expr<'V>
    abstract member Handle<'W> : Zir<'I, 'O, 'W> * (Expr<'W> -> Zir<'I, 'O, 'V>) -> Expr<'V>
    abstract member Handle<'M> : Zir<'I, 'M, 'V> * Zir<'M, 'O, 'V> -> Expr<'V>
    abstract member Handle : Eq<unit, 'V> * Expr<bool> * Zir<'I, 'O, unit> -> Expr<'V>
    abstract member Handle : Expr<bool> * Zir<'I, 'O, 'V> * Zir<'I, 'O, 'V> -> Expr<'V>
    
and Yield<'I, 'O>(o : Expr<'O>) =
    interface Zir<'I, 'O, unit> with
        member self.Invoke handler = handler.Handle(refl(), o)

and Done<'I, 'O, 'V>(v : Expr<'V>) =
    interface Zir<'I, 'O, 'V> with
        member self.Invoke handler = handler.Handle(v)

and NeedInput<'I, 'O>() =
    interface Zir<'I, 'O, 'I> with
        member self.Invoke handler = handler.Handle(refl())

and HasNext<'I, 'O>() =
    interface Zir<'I, 'O, bool> with
        member self.Invoke handler = handler.Handle(refl())

and Bind<'I, 'O, 'V, 'W>(z : Zir<'I, 'O, 'V>, f : Expr<'V> -> Zir<'I, 'O, 'W>) = 
    interface Zir<'I, 'O, 'W> with
        member self.Invoke handler = handler.Handle(z, f)

and Pipe<'I, 'M, 'O, 'V>(z1 : Zir<'I, 'M, 'V>, z2 : Zir<'M, 'O, 'V>) = 
    interface Zir<'I, 'O, 'V> with
        member self.Invoke handler = handler.Handle(z1, z2)

and While<'I, 'O>(pred : Expr<bool>, body : Zir<'I, 'O, unit>) = 
    interface Zir<'I, 'O, unit> with
        member self.Invoke handler = handler.Handle(refl(), pred, body)

and If<'I, 'O, 'V>(pred : Expr<bool>, then' : Zir<'I, 'O, 'V>, else' : Zir<'I, 'O, 'V>) = 
    interface Zir<'I, 'O, 'V> with
        member self.Invoke handler = handler.Handle(pred, then', else')

// helper functions
let yield' : Expr<'O> -> Zir<'I, 'O, unit> = 
    fun o -> new Yield<'I, 'O>(o) :> _

let next<'I, 'O> : Zir<'I, 'O, 'I> = 
    new NeedInput<'I, 'O>() :> _

let hasNext<'I, 'O> : Zir<'I, 'O, bool> = 
    new HasNext<'I, 'O>() :> _

let ret : Expr<'V> -> Zir<'I, 'O, 'V> =
    fun v -> new Done<'I, 'O, 'V>(v) :> _
    
let rec bind : Zir<'I, 'O, 'V> -> (Expr<'V> -> Zir<'I, 'O, 'W>) -> Zir<'I, 'O, 'W> =
    fun z f -> new Bind<'I, 'O, 'V, 'W>(z, f) :> _

let (>>>) : Zir<'I, 'M, 'V> -> Zir<'M, 'O, 'V> -> Zir<'I, 'O, 'V> =
    fun z1 z2 -> new Pipe<'I, 'M, 'O, 'V>(z1, z2) :> _

let do' : Expr<'V> -> Zir<'I, 'O, 'V> =
    ret 

let if' : Expr<bool> -> Zir<'I, 'O, 'V> -> Zir<'I, 'O, 'V> -> Zir<'I, 'O, 'V> =
    fun pred then' else' ->
        new If<'I, 'O, 'V>(pred, then', else') :> _

let ifthen : Expr<bool> -> Zir<'I, 'O, unit> -> Zir<'I, 'O, unit> =
    fun pred then' -> if' pred then' (ret <@ () @>)


// Builder type
type ZirBuilder() =
    member self.Return (v : Expr<'V>) : Zir<'I, 'O, 'V> = ret v
    member self.Bind(z : Zir<'I, 'O, 'V>, f : Expr<'V> -> Zir<'I, 'O, 'W>) : Zir<'I, 'O, 'W> =
        bind z f
    member self.While(pred : unit -> Expr<bool>, body : Zir<'I, 'O, unit>) : Zir<'I, 'O, unit> =
        new While<'I, 'O>(pred (), body) :> _
    member self.Delay(f : unit -> Zir<'I, 'O, 'V>) : Zir<'I, 'O, 'V> = 
        f ()
    member self.Combine(first : Zir<'I, 'O, unit>, second : Zir<'I, 'O, 'V>) : Zir<'I, 'O, 'V> = 
        self.Bind(first, fun _ -> second)

let zir = new ZirBuilder()

let rec compile<'I, 'O, 'V> : Expr<CancellationToken> -> Expr<bool> -> Expr<'I> -> (Expr<'O> -> Expr<unit>) -> Zir<'I, 'O, 'V> -> Expr<'V> =
    fun token hasNext input output z ->
        z.Invoke {
            new Handler<'I, 'O, 'V> with
                member self.Handle(eq : Eq<unit, 'V>, o : Expr<'O>) : Expr<'V> = 
                    cast eq <@ let o' = %o in (% lambda (fun o' -> output o')) o' @>
                member self.Handle(v : Expr<'V>) : Expr<'V> =
                    v
                member self.Handle(eq : Eq<'I, 'V>) : Expr<'V> =
                    cast eq input
                member self.Handle(eq : Eq<bool, 'V>) : Expr<'V> =
                    cast eq hasNext
                member self.Handle<'W>(z : Zir<'I, 'O, 'W>, f : Expr<'W> -> Zir<'I, 'O, 'V>) : Expr<'V> = 
                    <@ let w = (% compile token hasNext input output z)
                       (%token).ThrowIfCancellationRequested()
                       (% lambda (fun w -> compile token hasNext input output (f w))) w
                    @>
                member self.Handle(eq : Eq<unit, 'V>, pred : Expr<bool>, body : Zir<'I, 'O, unit>) : Expr<'V> = 
                    cast eq
                        <@ while %pred do
                            (% compile<'I, 'O, unit> token hasNext input output body)
                        @>  
                member self.Handle(pred : Expr<bool>, then' : Zir<'I, 'O, 'V>, else' : Zir<'I, 'O, 'V>) : Expr<'V> = 
                    <@ if %pred then (% compile token hasNext input output then') else (% compile token hasNext input output else') @>
                member self.Handle<'M>(z1 : Zir<'I, 'M, 'V>, z2 : Zir<'M, 'O, 'V>) : Expr<'V> = 
                    <@
                        let queue = new ConcurrentQueue<'M>()
                        let tcs = CancellationTokenSource.CreateLinkedTokenSource(%token)
                        let token = tcs.Token
                        let task1 = Static.run (fun () -> (% lambda2 (fun (queue : Expr<ConcurrentQueue<'M>>) token -> compile<'I, 'M, 'V> token hasNext input (fun v -> <@ (%queue).Enqueue %v @>) z1)) queue token)
                        let task2 = Static.run (fun () -> (% lambda2 (fun (queue : Expr<ConcurrentQueue<'M>>) token -> compile<'M, 'O, 'V> token <@ (%queue).Count <> 0 @> <@ Static.get (%queue) @> output z2)) queue token)
                        let tasks = [|task1; task2|]
                        let index = Task.WaitAny(task1, task2)
                        tcs.Cancel()
                        tasks.[index].Result
                    @>
                    
        }

// example
let example1 : Zir<int, string, unit> = 
    zir {
        while <@ true @> do
            let! flag = hasNext
            let! _ = 
                ifthen flag <| zir { 
                    let! x = next
                    let! _ = yield' <@ string (%x + 1) @>
                    return <@ () @>
                }
            return <@ () @>
        return <@ () @>
    }

let example2 : Zir<string, (int * int), unit> = 
    zir {
        let! counter = do' <@ ref 0 @>
        while <@ !(%counter) < 100 @> do
            let! flag = hasNext
            let! _ = 
                ifthen flag <| zir { 
                    let! x = next
                    let! _ = yield' <@ (!(%counter), System.Int32.Parse %x) @>
                    let! _ = do' <@ incr %counter @>
                    return <@ () @>
                }
            return <@ () @>
        return <@ () @>
    }
let example : Zir<int, (int * int), unit> = example1 >>> example2


let example' : Expr<ConcurrentQueue<int>> -> Expr<CancellationToken> -> Expr<Task<unit>> = 
    fun queue token -> 
        <@ 
            Static.run (fun () -> (% compile token <@ (%queue).Count <> 0 @> <@ Static.get (%queue) @> (fun o -> <@ printfn "%A" %o @>) example))
        @>

let f = QuotationCompiler.ToFunc (lambda2 (fun queue token -> example' queue token))

let queue = new ConcurrentQueue<int>()
for i = 1 to 100 do
    queue.Enqueue(i)
    ()

let tcs = new CancellationTokenSource()
let task = f () queue tcs.Token


