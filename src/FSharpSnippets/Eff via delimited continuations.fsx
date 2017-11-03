// The Eff Monad via delimited continuations
// based on http://kcsrk.info/papers/eff_ocaml_ml16.pdf

// Basic types
type Effect = 
    abstract UnPack : Lambda -> Effect 
and Lambda =
        abstract Invoke<'X> : ('X -> Effect) -> ('X -> Effect)

and Eff<'U, 'A when 'U :> Effect> = 
    Eff of (('A -> Effect) -> Effect)

and Done<'A>(v : 'A) =
    member self.Value = v
    interface Effect with
        member self.UnPack(_ : Lambda) : Effect =
                new Done<'A>(v) :> _
 
// Monad instance
type EffBuilder() = 
    member self.Return<'U, 'A when 'U :> Effect> (v : 'A) : Eff<'U, 'A> = 
        Eff (fun k -> k v)
    member self.Bind<'U, 'A, 'B when 'U :> Effect>(eff : Eff<'U, 'A>, f : 'A -> Eff<'U, 'B>) : Eff<'U, 'B> = 
        Eff (fun k -> let (Eff effK) = eff in effK (fun v -> let (Eff effK') = f v in effK' k))

let eff = new EffBuilder()

module Eff =
    let done' (v : 'A) : Effect = 
        new Done<'A>(v) :> _ 
    let shift (f : ('A -> Effect) -> Effect) : Eff<'U, 'A> = 
        Eff (fun k -> f k)

open Eff

// State Effect
type State<'S> = inherit Effect
type Put<'S>(v : 'S, k : unit -> Effect) =
    interface State<'S> with
        member self.UnPack(lambda : Lambda) : Effect =             
            new Put<'S>(v, lambda.Invoke<unit> k) :> _
    member self.Value = v
    member self.K = k
type Get<'S>(k : 'S -> Effect) =
    interface State<'S> with
        member self.UnPack(lambda : Lambda) : Effect =             
                new Get<'S>(lambda.Invoke<'S> k) :> _
    member self.K = k

let get<'U, 'S when 'U :> State<'S>>() : Eff<'U, 'S> = 
    shift (fun k -> new Get<'S>(k) :> _)
let put<'U, 'S when 'U :> State<'S>> : 'S -> Eff<'U, unit> = fun s ->
    shift (fun k -> new Put<'S>(s, k) :> _)

// Reader Effect
type Reader<'E> = inherit Effect
type Ask<'E>(k : 'E -> Effect) =
    interface Reader<'E> with
        member self.UnPack(lambda : Lambda) : Effect =
                new Ask<'E>(lambda.Invoke<'E> k) :> _
    member self.K = k

let ask<'U, 'E when 'U :> Reader<'E>>() : Eff<'U, 'E> = 
    shift (fun k -> new Ask<'E>(k) :> _)

// interpreters
let rec runState<'U, 'S, 'A when 'U :> State<'S>> 
    : 'S -> Eff<'U, 'A> -> Eff<'U, 'S * 'A> = 
    fun state eff ->
        let rec loop : ('S * 'A -> Effect) -> 'S -> Effect -> Effect = fun k state effect ->
            match effect with
            | :? Get<'S> as get -> loop k state (get.K state) 
            | :? Put<'S> as put -> loop k put.Value (put.K ())
            | :? Done<'A> as done' -> k (state, done'.Value)
            | _ -> effect.UnPack {
                new Lambda with
                    member self.Invoke<'X> (k' : 'X -> Effect) = 
                        fun x -> loop k state (k' x)
            }
        let (Eff effK) = eff
        let effect = effK done'
        Eff (fun k -> loop k state effect)

let rec runReader<'U, 'E, 'A when 'U :> Reader<'E>> 
    : 'E -> Eff<'U, 'A> -> Eff<'U, 'A> = 
    fun env eff ->
        let rec loop : ('A -> Effect) -> 'E -> Effect -> Effect = fun k env effect ->
            match effect with
            | :? Ask<'E> as ask -> loop k env (ask.K env) 
            | :? Done<'A> as done' -> k done'.Value
            | _ -> effect.UnPack {
                new Lambda with
                    member self.Invoke<'X> (k' : 'X -> Effect) = 
                        fun x -> loop k env (k' x)
            }
        let (Eff effK) = eff
        let effect = effK done'
        Eff (fun k -> loop k env effect)

let rec run<'U, 'A when 'U :> Effect> : Eff<'U, 'A> -> 'A = 
    fun eff ->
        let (Eff effK) = eff
        let effect = effK done'
        match effect with
        | :? Done<'A> as done' -> done'.Value
        | _ -> failwithf "Unhandled effect %A" effect

// Example
// val example : unit -> Eff<'U,int> when 'U :> Reader<int> and 'U :> State<int>
let example () =
    eff {
        do! put 1
        let! y = ask ()
        let! x = get ()
        return x + y
    }

type ExEffect = inherit State<int> inherit Reader<int>

(run << runReader 1 << runState<ExEffect, _, _> 0) (example ()) // (1, 2)


