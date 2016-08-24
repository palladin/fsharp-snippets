// A simple extension method for asynchronous non-deterministic computations.

open System

[<AutoOpen>]
module AsyncEx = 
    type private SuccessException<'T>(value : 'T) =
        inherit Exception()
        member self.Value = value

    type Microsoft.FSharp.Control.Async with
        // efficient raise
        static member Raise (e : #exn) = Async.FromContinuations(fun (_,econt,_) -> econt e)
  
        static member Choice<'T>(tasks : seq<Async<'T option>>) : Async<'T option> =
            let wrap task =
                async {
                    let! res = task
                    match res with
                    | None -> return None
                    | Some r -> return! Async.Raise <| SuccessException r
                }

            async {
                try
                    do!
                        tasks
                        |> Seq.map wrap
                        |> Async.Parallel
                        |> Async.Ignore

                    return None
                with 
                | :? SuccessException<'T> as ex -> return Some ex.Value
            }

// example 1

let delay interval result =
    async {
        do! Async.Sleep interval
        return! async {
            printfn "returning %A after %d ms." result interval
            return result }
    }

[ delay 100 None ; delay 1000 (Some 1) ; delay 500 (Some 2) ] |> Async.Choice |> Async.RunSynchronously
                    
// example 2

/// parallel existential combinator
let exists (f : 'T -> Async<bool>) (ts : seq<'T>) : Async<bool> =
    let wrapper t = async { let! r = f t in return if r then Some () else None }
                
    async {
        let! r = ts |> Seq.map wrapper |> Async.Choice
        return r.IsSome
    }

#time
[1..500] |> Seq.exists (fun i -> Threading.Thread.Sleep 10; i = 500)
[1..500] |> exists (fun i -> async { let! _ = Async.Sleep 10 in return i = 500 }) |> Async.RunSynchronously