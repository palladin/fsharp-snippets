// Examples of Infinite Streams defined using a lazy fixed-point combinator.

let force (value : Lazy<_>) = value.Force()

// Infinite Stream
type Stream<'T> = Cons of 'T * Lazy<Stream<'T>>
let head (Cons (h, _)) = h
let tail (Cons (_, t)) = force t 

// Lazy fixed-point
let fix : (Lazy<'T> -> 'T) -> Lazy<'T> = fun f ->
    let rec x = lazy (f x) in x 

// Examples  
let ones = fix (fun x -> Cons (1, x))
let map f = fix (fun f' x -> Cons (f (head x), lazy(force f' (tail x)))  )
let nats = fix (fun x -> Cons (1, lazy ( (force (map ((+) 1))) (force x)  )))