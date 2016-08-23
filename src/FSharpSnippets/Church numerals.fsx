// Church numerals via rank-2 polymorphism

type Church =
    abstract Apply<'T> : ('T -> 'T) * 'T -> 'T

let zero = 
    { 
        new Church with
            member self.Apply(f, x) = x 
    }

let succ (nat : Church) = 
    { 
        new Church with
            member self.Apply(f, x) = f <| nat.Apply(f, x) 
    }

let one = succ zero
let two = succ one

let (<+>) (first : Church) (second : Church) = 
    first.Apply(succ, second) 
let (<*>) (first : Church) (second : Church) = 
    first.Apply((<+>) second, zero) 
let (<^>) (first : Church) (second : Church) = 
    second.Apply((<*>) first, one) 

let toInt (nat : Church) = 
    nat.Apply((+)1, 0)  

let three = two <+> one
let six = three <*> two
let eight = two <^> three
   
three |> toInt // 3
six |> toInt // 6
eight |> toInt // 8