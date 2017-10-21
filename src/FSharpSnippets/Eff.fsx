// The Eff Monad
// based on http://okmij.org/ftp/Computation/free-monad.html

// Helper Equality type
module Eq = 

    type Eq<'A, 'B> = private Refl of ('A -> 'B) * ('B -> 'A)
    
    let refl<'A> () : Eq<'A, 'A> = Refl (id, id)
    let sym : Eq<'A, 'B> -> Eq<'B, 'A> = fun (Refl (f, g)) -> Refl (g, f)
    let trans : Eq<'A, 'B> -> Eq<'B, 'C> -> Eq<'A, 'C> = 
        fun (Refl (f, g)) (Refl (h, k)) -> Refl (f >> h, k >> g)
    let cast : Eq<'A, 'B> -> 'A -> 'B = fun (Refl (f, _)) -> f

open Eq

// Basic GADT encoding
type Effect = interface end

[<AbstractClass>]
type Eff<'U, 'A when 'U :> Effect>() = 
    abstract Invoke<'R> : EffHandler<'U, 'A, 'R> -> 'R 

and EffHandler<'U, 'A, 'R when 'U :> Effect> =
    abstract Handle : 'A -> 'R 
    abstract Handle<'X> : Effect * ('X -> Eff<'U, 'A>) -> 'R
 
and Pure<'U, 'A when 'U :> Effect>(a : 'A) = 
    inherit Eff<'U, 'A>()
        override self.Invoke handler = 
            handler.Handle a
 
 and Impure<'U, 'X, 'A when 'U :> Effect>(effect : Effect, f : 'X -> Eff<'U, 'A>) = 
    inherit Eff<'U, 'A>()
        override self.Invoke handler = 
            handler.Handle<'X>(effect, f)
 
// Monad instance
type EffBuilder() = 
    member self.Return<'U, 'A when 'U :> Effect> (x : 'A) : Eff<'U, 'A> = new Pure<'U, 'A>(x) :> _
    member self.Bind<'U, 'A, 'B when 'U :> Effect>(eff : Eff<'U, 'A>, f : 'A -> Eff<'U, 'B>) : Eff<'U, 'B> = 
        eff.Invoke<Eff<'U, 'B>> {
            new EffHandler<'U, 'A, Eff<'U, 'B>> with
                member self'.Handle x = f x
                member self'.Handle<'X>(effect, f') = 
                    new Impure<'U, 'X, 'B>(effect, fun x -> self.Bind(f' x, f)) :> _
        }

let eff = new EffBuilder()

// State Effect
type State<'S> = inherit Effect
type State<'S, 'A> = 
    abstract Invoke<'R> : StateHandler<'S, 'A, 'R> -> 'R 
    inherit State<'S>
and StateHandler<'S, 'A, 'R> =
    abstract Handle : 'S * Eq<unit, 'A> -> 'R 
    abstract Handle : Eq<'S, 'A> -> 'R
type Get<'S>() = 
    interface State<'S, 'S> with
        member self.Invoke<'R> (handler : StateHandler<'S, 'S, 'R>) = 
            handler.Handle(refl<'S>())
type Put<'S>(state : 'S) = 
    member self.State = state
    interface State<'S, unit> with
        member self.Invoke<'R> (handler : StateHandler<'S, unit, 'R>) = 
            handler.Handle(state, refl<unit>())

let get<'U, 'S when 'U :> State<'S>>() : Eff<'U, 'S> = 
    new Impure<'U, 'S, 'S>(new Get<'S>(), fun x -> new Pure<'U, 'S>(x) :> _) :> _
let put<'U, 'S when 'U :> State<'S>> : 'S -> Eff<'U, unit> = 
    fun s -> new Impure<'U, unit, unit>(new Put<'S>(s), fun _ -> new Pure<'U, unit>(()) :> _) :> _ 

// Reader Effect
type Reader<'E> = inherit Effect
type Reader<'E, 'A> = 
    abstract Invoke<'R> : ReaderHandler<'E, 'A, 'R> -> 'R 
    inherit Reader<'E>
and ReaderHandler<'E, 'A, 'R> =
    abstract Handle : Eq<'E, 'A> -> 'R
type Ask<'E>() = 
    interface Reader<'E, 'E> with
            member self.Invoke<'R> (handler : ReaderHandler<'E, 'E, 'R>) = 
                handler.Handle(refl<'E>())

let ask<'U, 'E when 'U :> Reader<'E>>() : Eff<'U, 'E> = 
    new Impure<'U, 'E, 'E>(new Ask<'E>(), fun x -> new Pure<'U, 'E>(x) :> _) :> _

// interpreters
let rec runState<'U, 'S, 'A when 'U :> State<'S>> 
    : 'S -> Eff<'U, 'A> -> Eff<'U, 'S * 'A> = 
    fun state eff ->
        eff.Invoke<Eff<'U, 'S * 'A>> {
            new EffHandler<'U, 'A, Eff<'U, 'S * 'A>> with
                member self.Handle x = new Pure<'U, 'S * 'A>((state, x)) :> _ 
                member self.Handle<'X>(effect, f : 'X -> Eff<'U, 'A>) = 
                    match effect with
                    | :? State<'S, 'X> as stateEffect ->
                        stateEffect.Invoke<Eff<'U, 'S * 'A>> {
                            new StateHandler<'S, 'X, Eff<'U, 'S * 'A>> with
                                member self.Handle(state' : 'S, eq : Eq<unit, 'X>) = 
                                    let eff' = f (cast eq ())
                                    runState state' eff'
                                member self.Handle(eq : Eq<'S, 'X>) = 
                                    let eff' = f (cast eq state)
                                    runState state eff'
                        }
                    | _ -> new Impure<'U, 'X, 'S * 'A>(effect, fun x -> runState state (f x)) :> _
        }

let rec runReader<'U, 'E, 'A when 'U :> Reader<'E>> 
    : 'E -> Eff<'U, 'A> -> Eff<'U, 'A> = 
    fun env eff ->
        eff.Invoke<Eff<'U, 'A>> {
            new EffHandler<'U, 'A, Eff<'U, 'A>> with
                member self.Handle x = new Pure<'U, 'A>(x) :> _ 
                member self.Handle<'X>(effect, f : 'X -> Eff<'U, 'A>) = 
                    match effect with
                    | :? Reader<'E, 'X> as readerEffect ->
                        readerEffect.Invoke<Eff<'U, 'A>> {
                            new ReaderHandler<'E, 'X, Eff<'U, 'A>> with
                                member self.Handle(eq : Eq<'E, 'X>) = 
                                    let eff' = f (cast eq env)
                                    runReader env eff'
                        }
                    | _ -> new Impure<'U, 'X, 'A>(effect, fun x -> runReader env (f x)) :> _
        }

let rec run<'U, 'A when 'U :> Effect> : Eff<'U, 'A> -> 'A = 
    fun eff ->
        eff.Invoke<'A> {
            new EffHandler<'U, 'A, 'A> with
                member self.Handle x = x
                member self.Handle<'X>(effect, f : 'X -> Eff<'U, 'A>) = 
                    failwith "Unhandled effect"
        }

// Example
// val example : unit -> Eff<'U,int> when 'U :> Reader<int> and 'U :> State<int>
let example () =
    eff {
        do! put 1
        let! x = get ()
        let! y = ask ()
        return x + y
    }

type ExEffect = inherit State<int> inherit Reader<int>


(run << runReader 1 << runState<ExEffect, _, _> 0) (example ()) // (1, 2)


