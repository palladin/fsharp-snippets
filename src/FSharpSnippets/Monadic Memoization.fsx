// Modular memoization within a pure functional setting that is implemented as a convenient computation builder.

// Inspired by http://www.cs.utexas.edu/~wcook/Drafts/2006/MemoMixins.pdf

// State Monad combined with Continuation Monad (StateT Monad transformer)
type StateContMonad<'s, 'a, 'r> = StateContMonad of ('s -> ('s -> 'a -> 'r) -> 'r)

// Computation Builder
type StateContBuilder() =
    member self.Return value = 
        StateContMonad (fun state k -> k state value)
    member self.Bind(StateContMonad contStateMonad, f) = 
        StateContMonad (fun state k -> 
            contStateMonad state (fun state' value -> 
                let (StateContMonad contMonad') = f value
                contMonad' state' k))
    member self.Delay( f : unit -> StateContMonad<'s, 'a, 'r> ) = 
        StateContMonad (fun state k -> 
            let (StateContMonad contStateMonad) = f ()
            contStateMonad state k)
 
let memo = new StateContBuilder()
 
// Tell me Y 
let rec Y f v = f (Y f) v

// Map functions
let check (value : 'a) : StateContMonad<Map<'a, 'r>, option<'r>, 'r> = 
    StateContMonad (fun map k -> k map (Map.tryFind value map))

let store (argument : 'a, result : 'r) : StateContMonad<Map<'a, 'r>, unit, 'r> = 
    StateContMonad (fun map k -> k (Map.add argument result map) ())

// Memoization Mixin
let memoize f argument =
    memo {
        let! checkResult = check argument
        match checkResult with
        | Some result -> return result
        | None ->
            let! result = f argument
            do! store (argument, result)
            return result
    }


let execute f n = 
    let (StateContMonad contStateMonad) = Y (memoize << f) n
    contStateMonad Map.empty (fun _ value -> value)
 
// Example
let big (value : int) = new System.Numerics.BigInteger(value)

let fib f n =
    if n = big 0 then memo { return big 0 }
    elif n = big 1 then memo { return big 1 }
    else
        memo {
            let! nMinus1Fib = f (n - big 1)
            let! nMinus2Fib = f (n - big 2)
            return nMinus1Fib + nMinus2Fib
        }
 
execute fib (big 100000)