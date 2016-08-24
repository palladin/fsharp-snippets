// A pattern for programming with generic folds (catamorphisms). Based on the classic "Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire" (1991) (Meijer, Fokkinga, Paterson).

// http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.41.125

type ListF<'a, 'b> = Empty | Cons of 'a * 'b
type List<'a> = InL of ListF<'a, List<'a>> with
    member self.Out() = let (InL x) = self in x
type PeanoF<'b> = Zero | Suc of 'b
type Peano = InP of PeanoF<Peano> with
    member self.Out() = let (InP x) = self in x

let inline out x = (^MF : (member Out : unit -> ^F) (x))

// Binary Functor
type BiFunctor = F with
    static member ($) (F, x:ListF<_, _>) = 
        fun f g -> 
            match x with
            | Empty -> Empty
            | Cons (x, xs) -> Cons (f x, g xs)
    static member ($) (F, x:PeanoF<_>) = 
        fun f g -> 
            match x with
            | Zero -> Zero
            | Suc x -> Suc (g x)

let inline bmap f g x = (F $ x) f g

// (|φ|)
let inline cata phi x = 
    let rec cata' x = 
        phi (bmap id cata' (out x))
    cata' x

// Example
let threeP = InP (Suc (InP (Suc (InP (Suc (InP Zero))))))
let threeL = InL (Cons (1, (InL (Cons (2, (InL (Cons (3, (InL Empty)))))))))

cata (fun x -> match x with Suc n -> n + 1 | Zero -> 0) threeP // 3
cata (fun x -> match x with Cons (x, l) -> x :: l| Empty -> []) threeL // [1; 2; 3]