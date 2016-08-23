// The repmin problem is to replace all elements of a tree of numbers by the minimum element, making only a single pass over the original tree. Repmin is a very ingenious example of Circular Programming.

// For more info
// http://www.springerlink.com/content/g74174vvl1861605/
// http://www.haskell.org/haskellwiki/Circular_programming

// Helper functions
let force (value : Lazy<_>) = value.Force()
let lazyMap f l = lazy (f (force l))

// Generic feedback loop function
let trace f input = 
    let rec x = lazy (f input (lazyMap snd x)) in fst (force x)

type Tree<'a> = L of 'a | B of Tree<'a> * Tree<'a>

// Copy the original tree - with patched Leaf nodes
let rec copy (tree : Tree<int>) (m : Lazy<int>) : (Tree<Lazy<int>> * int) = 
    match tree with
    | L a -> (L m, a)
    | B (l, r) ->
         let (l', ml) = copy l m
         let (r', mr) = copy r m
         (B (l', r'), min ml mr)

let repmin t = trace copy t

let rec print tree = 
    match tree with
    | L v -> sprintf "(L %A)" (force v)
    | B (l, r) -> sprintf "(B (%s, %s))" (print l) (print r)

// Example
print (repmin (B (B (L -1, L 2), L 1))) // "(B ((B ((L -1), (L -1))), (L -1)))"