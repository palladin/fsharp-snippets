// Delimited continuations encoded as a parameterized continuation monad.

type Cont<'T, 'S, 'R> = Cont of (('T -> 'S) -> 'R)

type ContBuilder() = 
    member self.Return x = Cont (fun k -> k x)
    member self.Bind (c : Cont<_, _, _>, f : _ -> Cont<_, _, _>) =
        Cont (fun k -> let (Cont contf) = c in contf (fun v -> let (Cont contf') = f v in contf' k))

let delim = new ContBuilder()

let run (c : Cont<_, _, _>) = let (Cont contf) = c in contf id

let shift f  = Cont (fun k -> f k)

// Cartesian product example
let result = 
    delim {
        let! x = shift (fun k -> [1; 2; 3] |> List.collect k)
        let! y = shift (fun k -> ["a"; "b"; "c"] |> List.collect k)
        return [(x, y)]
    } |> run // [(1, "a"); (1, "b"); (1, "c"); (2, "a"); (2, "b"); (2, "c"); (3, "a"); (3, "b"); (3, "c")]

// Error example
let test n : Cont<int option, int option, int option> =
    delim {
        do! shift (fun k -> if n = 0 then None else k ())
        return Some (42 / n)
    } 

test 0 |> run // None