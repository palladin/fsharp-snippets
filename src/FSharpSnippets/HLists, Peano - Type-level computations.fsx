// An experiment on type-level computations.

type HList = interface end
and HNil = HNil with
    static member inline (|*|) (f, HNil) = f $ HNil
    interface HList
and HCons<'a, 'b when 'b :> HList> = HCons of 'a * 'b with
    static member inline (|*|) (f, HCons(x, xs)) = f $ HCons(x, xs) 
    interface HList 

type Peano = interface end
and Zero = Zero with
    static member inline (|*|) (f, Zero) = f $ Zero
    interface Peano
and Succ<'a when 'a :> Peano> = Succ of 'a  with
    static member inline (|*|) (f, Succ(x)) = f $ Succ(x) 
    interface Peano 

type Bool = interface end
and True = True with
    interface Bool
and False = False with
    interface Bool

let inline (^+^) head tail = HCons(head, tail)

// Examples

type Append = Append with
    static member ($) (Append, HNil) = id
    static member inline ($) (Append, HCons(x, xs)) = fun list ->
        HCons (x, (Append |*| xs) list)

type Length = Length with
    static member ($) (Length, HNil) = Zero
    static member inline ($) (Length, HCons(x, xs)) = Succ (Length |*| xs) 

type Even = Even with
    static member ($) (Even, Zero) = True
    static member inline ($) (Even, Succ (x)) = Odd |*| x 
and Odd = Odd with
    static member ($) (Odd, Zero) = False
    static member inline ($) (Odd, Succ (x)) = Even |*| x 

let first = 1 ^+^ '1' ^+^ HNil
let second =  "1" ^+^ true ^+^ HNil

// result : HCons<int,HCons<char,HCons<string,HCons<bool,HNil>>>>
let result = (Append $ first) second // HCons (1,HCons ('1',HCons ("1",HCons (true,HNil))))
// length : Succ<Succ<Succ<Succ<Zero>>>>
let length = Length $ result // Succ (Succ (Succ (Succ Zero)))
let _ : True = Even $ length // ok
let _ : False = Even $ length // type error
let _ : True = Odd $ length // type error
let _ : False = Odd $ length // ok