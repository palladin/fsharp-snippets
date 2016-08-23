// Yet another attempt of mine to "haskellify" my F# coding.


#r "packages/FSPowerPack.Core.Community.2.0.0.0/Lib/Net40/FSharp.PowerPack.dll"

open System

// Generic container of 'T 
// Also parameterized by 'TypeClass : (new : unit -> 'TypeClass) 
// to implicit get a 'TypeClass instance (like passing the type class dictionary)
// The idea is to encode Type Classes with subtype polymorphism and OOP Classes 
type Generic<'T, 'TypeClass when 'TypeClass : (new : unit -> 'TypeClass)> = interface end

type [<AbstractClass>] FunctorClass<'FunctorClass when 'FunctorClass :> FunctorClass<'FunctorClass> 
                                                  and  'FunctorClass : (new : unit -> 'FunctorClass)>() = 
    abstract FMap<'T, 'R> : ('T -> 'R) -> Generic<'T, 'FunctorClass> -> Generic<'R, 'FunctorClass>

type [<AbstractClass>] ApplicativeClass<'ApplicativeClass when 'ApplicativeClass :> ApplicativeClass<'ApplicativeClass> 
                                                  and  'ApplicativeClass : (new : unit -> 'ApplicativeClass)>() = 
    inherit FunctorClass<'ApplicativeClass>() 
    // abstract methods
    abstract Pure<'T> : 'T -> Generic<'T, 'ApplicativeClass>
    abstract Apply<'T, 'R> : Generic<'T -> 'R, 'ApplicativeClass> -> Generic<'T, 'ApplicativeClass> -> Generic<'R, 'ApplicativeClass>
    // Functor default implementation
    override this.FMap<'T, 'R> (f : 'T -> 'R) (fa : Generic<'T, 'ApplicativeClass>) : Generic<'R, 'ApplicativeClass> = 
        this.Apply (this.Pure f) fa

type [<AbstractClass>] MonadClass<'MonadClass when 'MonadClass :> MonadClass<'MonadClass> 
                                              and  'MonadClass : (new : unit -> 'MonadClass)>() =
    inherit ApplicativeClass<'MonadClass>() 
    // abstract methods
    abstract Return<'T> : 'T -> Generic<'T, 'MonadClass>
    abstract Bind<'T, 'R> : Generic<'T, 'MonadClass> * ('T -> Generic<'R, 'MonadClass>) -> Generic<'R, 'MonadClass>
    // Default implementations
    member this.Then<'T, 'R> ((ma : Generic<'T, 'MonadClass>), (mb : Generic<'R, 'MonadClass>)) : Generic<'R, 'MonadClass> = 
        this.Bind(ma, fun _ -> mb)
    // Applicative default implementation
    override this.Pure<'T> (value : 'T) : Generic<'T, 'MonadClass> = this.Return value
    override this.Apply<'T, 'R> (mf : Generic<'T -> 'R, 'MonadClass>) (ma : Generic<'T, 'MonadClass>) : Generic<'R, 'MonadClass> =
        this.Bind(mf, fun f -> this.Bind(ma, fun a -> this.Pure (f a)))

// Maybe Monad
type Maybe<'T> = None | Some of 'T with
    interface Generic<'T, MaybeClass> 
and MaybeClass() = 
    inherit MonadClass<MaybeClass>() with
        override this.Return<'T>(v : 'T) = Some v :> _
        override this.Bind<'T, 'R> ((m : Generic<'T, MaybeClass>), (f : ('T -> Generic<'R, MaybeClass>))) : Generic<'R, MaybeClass> = 
            match m :?> _ with
            | Some v -> f v
            | None -> None :> _

let maybe = new MaybeClass() :> MonadClass<MaybeClass>

// List Monad 
type ListMonadGeneric<'T> = ListMonadGeneric of LazyList<'T> with
    interface Generic<'T, ListMonadClass>    

and ListMonadClass() = 
    inherit MonadClass<ListMonadClass>() with
        override this.Return<'T>(v : 'T) = ListMonadGeneric (LazyList.ofList [v]) :> _
        override this.Bind<'T, 'R> ((m : Generic<'T, ListMonadClass>), (f : ('T -> Generic<'R, ListMonadClass>))) : Generic<'R, ListMonadClass> = 
            let (ListMonadGeneric list) = m :?> _ in 
                ListMonadGeneric (LazyList.ofSeq <| Seq.collect (fun v -> let (ListMonadGeneric list') = (f v) :?> _ in list') list) :> _

// ZipList Applicative Functor
type ListAppGeneric<'T> = ListAppGeneric of LazyList<'T> with
    interface Generic<'T, ListApplicativeClass>
and ListApplicativeClass() = 
    inherit ApplicativeClass<ListApplicativeClass>() with
        override this.Pure<'T> (v : 'T) = ListAppGeneric (LazyList.repeat v) :> _
        override this.Apply<'T, 'R> (ff : Generic<'T -> 'R, ListApplicativeClass>) (fa : Generic<'T, ListApplicativeClass>) : Generic<'R, ListApplicativeClass> = 
            let (ListAppGeneric listf) = ff :?> _ in 
            let (ListAppGeneric list) = fa :?> _ in 
                ListAppGeneric (LazyList.map (fun (f, a) -> f a) (LazyList.zip listf list)) :> _


//  Generic functions that operate over all Applicative Funtors
[<AutoOpen>]
module ApplicativeModule = 

    let pure<'T, 'ApplicativeClass when 'ApplicativeClass :> ApplicativeClass<'ApplicativeClass> 
                             and  'ApplicativeClass : (new : unit -> 'ApplicativeClass)> 
        (v : 'T) : Generic<'T, 'ApplicativeClass> = 
        (new 'ApplicativeClass()).Pure v

    let apply<'T, 'R, 'ApplicativeClass when 'ApplicativeClass :> ApplicativeClass<'ApplicativeClass> 
                                 and  'ApplicativeClass : (new : unit -> 'ApplicativeClass)> 
        (ff : Generic<'T -> 'R, 'ApplicativeClass>) (fa : Generic<'T, 'ApplicativeClass>) : Generic<'R, 'ApplicativeClass> =
        (new 'ApplicativeClass()).Apply ff fa

    let (<*>) ff fa = apply ff fa
    let ($) f fa = pure f <*> fa
    // Monoidal - pair
    let (<.>) fa fb = (fun a b -> (a, b)) $ fa <*> fb


//  Generic functions that operate over all Monads
[<AutoOpen>]
module MonadModule = 

    let unit<'T, 'MonadClass when 'MonadClass :> MonadClass<'MonadClass> 
                             and  'MonadClass : (new : unit -> 'MonadClass)> 
        (v : 'T) : Generic<'T, 'MonadClass> = 
        (new 'MonadClass()).Return v

    let bind<'T, 'R, 'MonadClass when 'MonadClass :> MonadClass<'MonadClass> 
                                 and  'MonadClass : (new : unit -> 'MonadClass)> 
        (m : Generic<'T, 'MonadClass>) (f : 'T -> Generic<'R, 'MonadClass>) : Generic<'R, 'MonadClass> = 
        (new 'MonadClass()).Bind(m, f)

    let (>>=) = bind
    let (>>) ma mb = ma >>= fun _ -> mb

    let rec sequence (list : Generic<'T, 'MonadClass> list) : Generic<'T list, 'MonadClass> = 
            match list with
            | [] -> unit []
            | m :: ms -> m >>= fun v -> sequence ms >>= fun vs -> unit (v :: vs)

    let mapM (f : 'T -> Generic<'R, 'MonadClass>) (list : 'T list) : Generic<'R list, 'MonadClass> = 
        (sequence << List.map f) list

    let rec filterM (p : 'T -> Generic<bool, 'MonadClass>) (list : 'T list) : Generic<'T list, 'MonadClass> =
        match list  with
        | [] -> unit []
        | x :: xs -> p x >>= fun b -> filterM p xs >>= fun ys -> if b then unit (x :: ys) else unit ys

// Examples

// Maybe Monad Examples
maybe { return 1 } >>= fun k -> maybe { return k + 1 } // Some 2
maybe { let! k = maybe { return 1 } in return k + 1 } // Some 2

sequence [maybe { return 1 }; maybe { return 2 }; maybe { return 3 }] // Some [1; 2; 3]
mapM (fun v -> maybe { return v * 2 }) [1 .. 5] //  Some [2; 4; 6; 8]
filterM (fun v -> maybe { return v % 2 = 0 }) [1..5] // Some [2; 4]

// ZipList example
let rec transpose (listoflist : LazyList<LazyList<'T>>) : Generic<LazyList<'T>, ListApplicativeClass> = 
    match listoflist with
    | LazyList.Nil -> pure LazyList.empty
    | LazyList.Cons (xs, xss) -> LazyList.cons $ (ListAppGeneric xs) <*> transpose xss

[[1; 2; 3]; [4; 5; 6]]
  |> LazyList.ofList
  |> LazyList.map LazyList.ofList
  |> transpose // result: ListAppGeneric (seq [seq [1; 4]; seq [2; 5]; seq [3; 6]])

// List Monad example
let onetoten = ListMonadGeneric (LazyList.ofList [1..3]) 
(fun a b -> sprintf "%d * %d = %d" a b (a * b)) $ onetoten <*> onetoten 
// result: ListMonadGeneric (seq ["1 * 1 = 1"; "1 * 2 = 2"; "1 * 3 = 3"; "2 * 1 = 2"; ...])