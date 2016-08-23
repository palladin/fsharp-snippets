// Clojure's Atoms are ref like structures, with the addition of (Compare And Swap) update semantics

open System.Threading

type Atom<'T when 'T : not struct>(value : 'T) =
    let refCell = ref value
    
    let rec swap f = 
        let currentValue = !refCell
        let result = Interlocked.CompareExchange<'T>(refCell, f currentValue, currentValue)
        if obj.ReferenceEquals(result, currentValue) then ()
        else Thread.SpinWait 20; swap f
        
    member self.Value with get() = !refCell
    member self.Swap (f : 'T -> 'T) = swap f
        
    

let atom value = 
    new Atom<_>(value)
    
let (!) (atom : Atom<_>) =  
    atom.Value
    
let swap (atom : Atom<_>) (f : _ -> _) =
    atom.Swap f

// example

let counter = atom (fun () -> 0)

let listOfIncrementAsync = 
  [ for _ in [1..1000000] do
      yield async { 
             swap counter (fun f -> (fun result () -> result + 1) <| f()) 
      } ]

listOfIncrementAsync |> Async.Parallel |> Async.RunSynchronously

let value = (!counter)() // returns 1000000

printfn "%d" value