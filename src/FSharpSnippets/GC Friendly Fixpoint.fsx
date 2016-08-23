// Fast and GC friendly fixpoint combinator.

#time

let rec Y f x = f (Y f) x

let Y' f x = 
    let r = ref Unchecked.defaultof<'a -> 'b>
    r := (fun x -> f !r x)
    f !r  x

let iter f x = if x = 100000000 then x else f (x + 1)

// Real: 00:00:01.504, CPU: 00:00:01.497, GC gen0: 572, gen1: 1, gen2: 0
Y iter 1
// Real: 00:00:00.769, CPU: 00:00:00.780, GC gen0: 0, gen1: 0, gen2: 0
Y' iter 1