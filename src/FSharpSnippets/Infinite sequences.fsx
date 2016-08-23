// Haskell-inspired infinite sequences


#r "packages/FSPowerPack.Core.Community.2.0.0.0/Lib/Net40/FSharp.PowerPack.dll"

let rec repeat n = LazyList.consDelayed n (fun () -> repeat n)
repeat 1 // seq [1; 1; 1; 1; ...]

let rec integersFrom n = LazyList.consDelayed n (fun () -> LazyList.map ((+) 1) <| integersFrom n) 
integersFrom 3 // seq [3; 4; 5; 6; ...]



let rec fibs() =
    let zipWith = LazyList.map2
    LazyList.consDelayed 0 (fun () -> LazyList.consDelayed 1 (fun () -> zipWith (+) <| fibs() 
                                                                                    <| (LazyList.tail <| fibs())))
fibs() |> LazyList.take 10 |> LazyList.toList // [0; 1; 1; 2; 3; 5; 8; 13; 21; 34]


let primes() = 
 let rec sieve integers =
    let prime, rest = LazyList.head integers, LazyList.tail integers
    LazyList.consDelayed prime (fun () -> sieve <| LazyList.filter (fun v -> v % prime > 0) rest)
 sieve (integersFrom 2)

primes() |> LazyList.take 10 |> LazyList.toList // [2; 3; 5; 7; 11; 13; 17; 19; 23; 29]

// http://rosettacode.org/wiki/Hamming_numbers
let rec hamming() = 
    let rec merge firstSeq secondSeq =
            let first, firstSeq' = LazyList.head firstSeq, LazyList.tail firstSeq
            let second, secondSeq' = LazyList.head secondSeq, LazyList.tail secondSeq
            match first, second with
            | _ when first < second -> LazyList.consDelayed first (fun () -> merge firstSeq' secondSeq)
            | _ when first > second -> LazyList.consDelayed second (fun () -> merge firstSeq secondSeq')
            | _ -> LazyList.consDelayed first (fun () -> merge firstSeq' secondSeq')

    LazyList.consDelayed 1 (fun () ->    merge  (LazyList.map ((*)2) <| hamming()) <| 
                                         merge  (LazyList.map ((*)3) <| hamming())  
                                                (LazyList.map ((*)5) <| hamming()))

     

hamming() |> LazyList.take 10 |> LazyList.toList // [1; 2; 3; 4; 5; 6; 8; 9; 10; 12]