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
type Eff<'F, 'A when 'F :> Effect>() = 
    abstract Invoke<'R> : EffHandler<'F, 'A, 'R> -> 'R 

and EffHandler<'F, 'A, 'R when 'F :> Effect> =
    abstract Handle : 'A -> 'R 
    abstract Handle<'X> : Effect * ('X -> Eff<'F, 'A>) -> 'R
 
and Pure<'F, 'A when 'F :> Effect>(a : 'A) = 
    inherit Eff<'F, 'A>()
        override self.Invoke handler = 
            handler.Handle a
 
 and Impure<'F, 'X, 'A when 'F :> Effect>(effect : Effect, f : 'X -> Eff<'F, 'A>) = 
    inherit Eff<'F, 'A>()
        override self.Invoke handler = 
            handler.Handle<'X>(effect, f)
 
// Monad instance
type EffBuilder() = 
    member self.Return<'F, 'A when 'F :> Effect> (x : 'A) : Eff<'F, 'A> = new Pure<'F, 'A>(x) :> _
    member self.Bind<'F, 'A, 'B when 'F :> Effect>(eff : Eff<'F, 'A>, f : 'A -> Eff<'F, 'B>) : Eff<'F, 'B> = 
        eff.Invoke<Eff<'F, 'B>> {
            new EffHandler<'F, 'A, Eff<'F, 'B>> with
                member self'.Handle x = f x
                member self'.Handle<'X>(effect, f') = 
                    new Impure<'F, 'X, 'B>(effect, fun x -> self.Bind(f' x, f)) :> _
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

let get<'F, 'S when 'F :> State<'S>>() : Eff<'F, 'S> = 
    new Impure<'F, 'S, 'S>(new Get<'S>(), fun x -> new Pure<'F, 'S>(x) :> _) :> _
let put<'F, 'S when 'F :> State<'S>> : 'S -> Eff<'F, unit> = 
    fun s -> new Impure<'F, unit, unit>(new Put<'S>(s), fun _ -> new Pure<'F, unit>(()) :> _) :> _ 

// Reader Effect
type Reader<'E> = inherit Effect
type Reader<'E, 'A> = inherit Reader<'E>
type Ask<'E> = inherit Reader<'E, 'E> 

let ask<'F, 'E when 'F :> Reader<'E>>() : Eff<'F, 'E> = 
    new Impure<'F, 'E, 'E>({ new Ask<'E> }, fun x -> new Pure<'F, 'E>(x) :> _) :> _

// interpreters
let rec runState<'F, 'S, 'A when 'F :> State<'S>> 
    : 'S -> Eff<'F, 'A> -> 'S * 'A = 
    fun state eff ->
        eff.Invoke<'S * 'A> {
            new EffHandler<'F, 'A, 'S * 'A> with
                member self.Handle x = (state, x)
                member self.Handle<'X>(effect, f : 'X -> Eff<'F, 'A>) = 
                    match effect with
                    | :? State<'S, 'X> as stateEffect ->
                        stateEffect.Invoke<'S * 'A> {
                            new StateHandler<'S, 'X, 'S * 'A> with
                                member self.Handle(state' : 'S, eq : Eq<unit, 'X>) = 
                                    let eff' = f (cast eq ())
                                    runState state' eff'
                                member self.Handle(eq : Eq<'S, 'X>) = 
                                    let eff' = f (cast eq state)
                                    runState state eff'
                        }
                    | _ -> failwith "Invalid effect"
        }

// Example
// val example : unit -> Eff<'F,int> when 'F :> Reader<int> and 'F :> State<int>
let example () =
    eff {
        do! put 1
        let! x = get ()
        let! y = ask ()
        return x + y
    }

type ExEffect = inherit State<int> inherit Reader<int>

runState<ExEffect, _, _> 0 (example ())


