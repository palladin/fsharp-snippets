
// Algebraic effects and handlers is a new modular approach for handling effectful computations in functional languages. Inspired from the paper "Handlers in action"

// http://homepages.inf.ed.ac.uk/slindley/papers/handlers-draft-march2013.pdf
// Quote from the paper
// Effect handler operation clauses generalise exception handler clauses
// by adding a continuation argument, providing support for arbitrary effects. An operation 
// clause is an exception clause if it ignores its continuation argument.

type Cont<'T, 'R> = Cont of ((('T -> 'R) * (exn -> 'R)) -> 'R)

type ContBuilder() = 
    member self.Return x = Cont (fun (k, _) -> k x)
    member self.ReturnFrom c = c
    member self.Bind (c : Cont<_, _>, f : _ -> Cont<_, _>) =
        Cont (fun (k, exk) -> let (Cont contf) = c in contf ((fun v -> let (Cont contf') = f v in contf' (k, exk)), exk))
    member self.TryWith (c : Cont<_, _>, f : exn -> Cont<_, _>) =
        Cont (fun (k, exk) -> 
                let (Cont contf) = c
                contf (k, (fun ex -> 
                    match (try Choice1Of2 (f ex) with ex -> Choice2Of2 ex) with
                    | Choice1Of2 (Cont contf') -> contf' (k, exk)
                    | Choice2Of2 ex -> exk ex)))
     member self.Delay (f : unit -> Cont<'T, 'R>) : Cont<'T, 'R> = 
        Cont (fun (k, exk) -> let (Cont contf) = f () in contf (k, exk))

let eff = new ContBuilder()

let run id (c : Cont<_, _>) = let (Cont contf) = c in contf (id, fun ex -> raise ex)

let shift f  = Cont (fun (k, exk) -> f k) 

// Basic state operations
type Put<'S, 'Ans>(v : 'S, k : unit -> 'Ans) =
    inherit System.Exception()
    member self.Value = v
    member self.K = k

type Get<'S, 'Ans>(k : 'S -> 'Ans) =
    inherit System.Exception()
    member self.K = k

let put (v : int) : Cont<unit, 'Ans> =
    Cont (fun (k, exk) ->  exk <| new Put<int,'Ans>(v, k))

let get () : Cont<int, 'Ans> = 
    Cont (fun (k, exk) ->  exk <| new Get<int,'Ans>(k))

// different ways of handling state
let pureState<'T, 'Ans> (c : Cont<'T, int -> 'Ans>) : Cont<'T, int -> 'Ans> = 
    eff {
        try
            return! c
        with 
            | :? Get<int, int -> 'Ans> as get -> return! Cont (fun _ s -> get.K s s)
            | :? Put<int, int -> 'Ans> as put -> return! Cont (fun _ _ -> put.K () put.Value)
    }
    
let refState<'T, 'Ans> (c : Cont<'T, 'Ans>) : Cont<'T, 'Ans> = 
    eff {
        let stateRef = ref 1
        try
            return! c
        with 
            | :? Get<int, 'Ans> as get -> return! Cont (fun _ -> get.K !stateRef)
            | :? Put<int, 'Ans> as put -> return! Cont (fun _ -> stateRef := put.Value; put.K () )
    }
    
let collectStates<'T, 'Ans> (c : Cont<'T, int -> ('T * int list)>) : Cont<'T, int -> ('T * int list)> = 
    eff {
        try
            return! c
        with 
            | :? Get<int, int -> ('T * int list)> as get -> 
                return! Cont (fun _ -> (fun s -> get.K s s))
            | :? Put<int, int -> ('T * int list)> as put -> 
                return! Cont (fun _ -> (fun _ ->
                                                let x = put.Value 
                                                let (v, xs) = put.K () x
                                                (v, x :: xs)))
    }

let logState<'T, 'Ans> (c : Cont<'T, 'Ans>) : Cont<'T, 'Ans> = 
    eff {
        try
            return! c
        with 
            | :? Put<int, 'Ans> as p -> 
                do printfn "%d" p.Value
                do! put (p.Value) // forward
                return! Cont (fun _ -> p.K ())
    }
 

// example
let test () = 
    eff {
        let! x = get ()
        do! put (x + 1)
        let! y = get ()
        do! put (y + y)
        return! get ()
    } 

    
test () |> logState |> pureState |> run (fun x -> (fun s -> (x, s))) |> (fun f -> f 1) // (4, 4)

test () |> logState |> refState |> run id // 4

test () |> logState |> collectStates |> run (fun x -> (fun s -> (x, []))) |> (fun f -> f 1) // (4, [2; 4])