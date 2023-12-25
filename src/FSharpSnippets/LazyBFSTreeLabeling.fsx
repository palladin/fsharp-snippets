

type StreamIn<'T> = Cons of Lazy<'T> * Stream<'T>
and Stream<'T> = Lazy<StreamIn<'T>>

let force : Lazy<'T> -> 'T = fun x -> x.Force()

let head : Stream<'T> -> 'T = 
    fun xs -> 
        match xs.Value with
        | Cons (x, _) -> x.Value

let tail : Stream<'T> -> Stream<'T> = fun xs ->
    match xs.Value with
    | Cons (_, xs') -> xs'

type Tree<'T> = Leaf of 'T | Branch of Tree<'T> * Tree<'T>

let rec treeMap : ('T -> 'R) -> Tree<'T> -> Tree<'R> = 
    fun f t -> 
        match t with
        | Leaf x -> Leaf (f x)
        | Branch (l, r) -> Branch (treeMap f l, treeMap f r)

let rec bfs : Tree<int> -> Stream<int> -> (Tree<Lazy<int>> * Stream<int>) = fun t s ->
    match t with
    | Leaf _ -> (Leaf <| lazy (head s), lazy (Cons (lazy (let c = head s in c + 1), tail s)))
    | Branch (l, r) -> 
        let (l', s') = bfs l (lazy (s |> tail |> force))
        let (r', s'') = bfs r s'
        (Branch (l', r'), lazy (Cons (lazy (head s), s'')))

let labelTree : Tree<int> -> Tree<int> = fun t ->
    let rec result = bfs t <| lazy (Cons (lazy 1, snd result))
    result |> fst |> treeMap force


let t = Branch (Branch (Leaf 1, Branch (Leaf 2, Leaf 3)), Branch (Leaf 4, Leaf 5))
labelTree t
