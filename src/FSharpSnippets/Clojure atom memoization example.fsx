// A Clojure inspired (race free) memoize function, that uses a mutable atom cell.

// Info: http://clojure.org/atoms

open System
open System.Threading

type Atom<'T when 'T : not struct>(value : 'T) =
    let refCell = ref value
    
    let rec swap f = 
        let currentValue = !refCell
        let result = Interlocked.CompareExchange<'T>(refCell, f currentValue, currentValue)
        if obj.ReferenceEquals(result, currentValue) then ()
        else Thread.SpinWait 20; swap f
        
    member self.Value with get() : 'T = !refCell
    member self.Swap (f : 'T -> 'T) : unit = swap f

module Atom =

    let atom value = new Atom<_>(value)
        
    let (!) (atom : Atom<_>) = atom.Value
    
    let swap (atom : Atom<_>) (f : _ -> _) = atom.Swap f

    let (|AtomCell|) (atomCell : Atom<'T>) = !atomCell


open Atom

let memoize f =
    let cacheAtom = atom Map.empty
    fun x ->
        match (!cacheAtom).TryFind(x) with
        | Some res -> res
        | None ->
             let res = f x
             swap cacheAtom (fun cache -> cache.Add(x,res))
             res


let rec fibonacci =  
  memoize(fun n -> if n <= 2 then 1 else fibonacci(n - 1) + fibonacci(n - 2))

fibonacci 20
Copy linkCopy sourceRaw viewLoad in New version