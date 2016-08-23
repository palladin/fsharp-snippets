// Generic Numeric Literals and Compile time Peano arithmetic

type Peano = interface end
and Zero = Zero with
    static member inline (|*|) (f, Zero) = f $ Zero
    interface Peano
and Succ<'a when 'a :> Peano> = Succ of 'a  with
    static member inline (|*|) (f, Succ(x)) = f $ Succ(x) 
    interface Peano 

module NumericLiteralG =
    let inline FromZero () = Zero
    let inline FromOne () = Succ Zero


type Add = Add with
    static member ($) (Add, Zero) = fun n -> n
    static member inline ($) (Add, Succ n) = fun n' ->
        Succ ((Add |*| n) n')

let inline (+) a b = (Add $ a) b

let three = 1G + 1G + 1G // Succ<Succ<Succ<Zero>>>
let three' = three + 0G // Succ<Succ<Succ<Zero>>>