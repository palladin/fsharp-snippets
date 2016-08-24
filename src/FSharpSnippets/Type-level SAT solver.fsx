// A type-level SAT solver in F#, inspired by https://gist.github.com/wouter-swierstra/0b6062c9660e751cd535

type Bool = interface end
and True = True with
    interface Bool
and False = False with
    interface Bool


type And = And with
    static member inline (?<-) (True, And, True) = True
    static member inline (?<-) (True, And, False) = False
    static member inline (?<-) (False, And, True) = False
    static member inline (?<-) (False, And, False) = False

and Or = Or with
    static member inline (?<-) (True, Or, True) = True
    static member inline (?<-) (True, Or, False) = True
    static member inline (?<-) (False, Or, True) = True
    static member inline (?<-) (False, Or, False) = False

and Not = Not with
    static member inline ($) (Not, True) = False
    static member inline ($) (Not, False) = True

let inline (<&&>) a b = (a ? (And) <- b )
let inline (<||>) a b = (a ? (Or) <- b )
let inline not a = Not $ a

let inline solve (p : ^T -> True) = typeof< ^T>.FullName


solve (fun a -> not a) // False
solve (fun a -> not (not a)) // True
solve (fun (a, b) -> a <&&> (not b)) // (True, False)
solve (fun a -> a <&&> (not a)) // type error