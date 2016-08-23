// x = f(x) encoded in F#

let force (value : Lazy<_>) = value.Force()

let fix f = let rec x = lazy (f x) in x

// Examples
let fac = fix (fun f x -> if x = 0 then 1 else x * force f (x - 1) )
let nums = fix (fun v -> seq { yield 0; yield! Seq.map ((+) 1) (force v) }) 

force fac 10 // 10! = 3628800
Seq.take 10 (force nums) // seq [0; 1; 2; 3; ...]