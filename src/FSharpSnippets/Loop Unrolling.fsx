// C++ style metaprogramming in F#


type Peano = interface end
and Zero = Zero with
    static member inline (|*|) (f, Zero) = f $ Zero
    interface Peano
and Succ<'a when 'a :> Peano> = Succ of 'a  with
    static member inline (|*|) (f, Succ(x)) = f $ Succ(x) 
    interface Peano


type PeanoToInt = PeanoToInt with
    static member inline ($) (PeanoToInt, Zero) = 0
    static member inline ($) (PeanoToInt, Succ (x)) = 1 + (PeanoToInt |*| x)

type Repeat = Repeat with
    static member inline ($) (Repeat, Zero) = fun f -> ()
    static member inline ($) (Repeat, (Succ (x) as p)) = fun f ->
        (Repeat |*| x) f
        f (PeanoToInt $ p) 

let four = Succ (Succ (Succ (Succ Zero)))
let inline repeat step f = (Repeat $ step) f

// Examples

repeat four (fun index -> printfn "index: %d" index)

// zero-out
let array = [|1..8|]
for i in 0 .. 4 .. array.Length - 1 do
    repeat four (fun index -> array.[i + (index - 1)] <- 0)