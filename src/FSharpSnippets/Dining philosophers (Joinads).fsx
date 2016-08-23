// A simple and declarative solution based on Joinads.

open System
open FSharp.Extensions.Joinads

// Init
let n = 5
let chopsticks = [| for i = 1 to n do yield new Channel<unit>() |]
let hungry = [| for i = 1 to n do yield new Channel<unit>() |]
let philosophers = [| "Plato"; "Konfuzius"; "Socrates"; "Voltaire"; "Descartes" |]

let randomDelay (r : Random) = System.Threading.Thread.Sleep(r.Next(1, 10) * 1000)

// Fork 
for i = 0 to n - 1 do
    let left = chopsticks.[i]
    let right = chopsticks.[(i+1) % n]
    let random = new Random()
    join {
        match! hungry.[i], left, right with
        | _, _, _ ->
            printfn "%s is eating" philosophers.[i] 
            randomDelay random
            left.Call(); right.Call()
            printfn "%s is thinking" philosophers.[i] 
    }
    
// Run
for chopstick in chopsticks do
    chopstick.Call()

let random = new Random()    
while true do
    hungry.[random.Next(0, n)].Call()
    randomDelay random