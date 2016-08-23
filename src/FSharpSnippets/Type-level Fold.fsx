// Generic Type-level Fold for Boolean Algebra


type Bool = interface end
and True = True with
    static member inline (|*|) (f, True) = (BoolFold ? (f) <- True)
    interface Bool
and False = False with
    static member inline (|*|) (f, False) = (BoolFold ? (f) <- False)
    interface Bool
and And<'a, 'b when 'a :> Bool and 'b :> Bool> = And of 'a * 'b  with
    static member inline (|*|) (f, x) = (BoolFold ? (f) <- x)
    interface Bool
and Or<'a, 'b when 'a :> Bool and 'b :> Bool> = Or of 'a * 'b  with
    static member inline (|*|) (f, x) = (BoolFold ? (f) <- x)
    interface Bool
and Not<'a when 'a :> Bool> = Not of 'a with
    static member inline (|*|) (f, x) = (BoolFold ? (f) <- x)
    interface Bool

and BoolFold = BoolFold with
    static member inline (?<-) (BoolFold, f, True) = f $ True
    static member inline (?<-) (BoolFold, f, False) = f $ False
    static member inline (?<-) (BoolFold, f, And(a, b)) = 
        let app = And (f |*| a, f |*| b)
        f $ app
    static member inline (?<-) (BoolFold, f, Or(a, b)) = 
        let app = Or (f |*| a, f |*| b)
        f $ app
    static member inline (?<-) (BoolFold, f, Not(a)) = 
        let app = Not (f |*| a)
        f $ app

type Eval = Eval with
    static member inline ($) (Eval, True) = True
    static member inline ($) (Eval, False) = False
    static member inline ($) (Eval, And(True, True)) = True 
    static member inline ($) (Eval, And(True, False)) = False 
    static member inline ($) (Eval, And(False, True)) = False
    static member inline ($) (Eval, And(False, False)) = False 
    static member inline ($) (Eval, Or(True, True)) = True 
    static member inline ($) (Eval, Or(True, False)) = True 
    static member inline ($) (Eval, Or(False, True)) = True
    static member inline ($) (Eval, Or(False, False)) = False  
    static member inline ($) (Eval, Not(True)) = False  
    static member inline ($) (Eval, Not(False)) = True  
       
type BitFlip = BitFlip with
    static member inline ($) (BitFlip, True) = False
    static member inline ($) (BitFlip, False) = True
    static member inline ($) (BitFlip, And(x, y)) = And(x, y)
    static member inline ($) (BitFlip, Or(x, y)) = Or(x, y) 
    static member inline ($) (BitFlip, Not(x)) = Not(x) 
        

let (<&>) a b = And (a, b)
let (<|>) a b = Or (a, b)
let inline fold f (x : ^R) = (BoolFold ? (f) <- x) 

//// Examples

let t = fold BitFlip False // t : True
let f = fold BitFlip True // f : False
let a = fold BitFlip (True <&> False) // And<False,True>
let e = fold Eval (True <&> False) // e : False
let n = fold Eval (True <|> False) // n : True
let k = fold Eval (Not True <|> False) // k : False
Previous VersionCopy linkCopy sourceRaw viewLoad in New version