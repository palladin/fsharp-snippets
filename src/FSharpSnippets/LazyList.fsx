// A LazyList implementation with tail recursive enumeration.

open System.Collections
open System.Collections.Generic

// LazyList dataType + Monoid Structure
type LazyList<'T> =   Empty
                    | Cons of 'T * (unit -> LazyList<'T>)
                    | Delay of (unit -> LazyList<'T>)
                    | Combine of LazyList<'T> * LazyList<'T> with
        interface IEnumerable<'T> with
            member self.GetEnumerator() =
                // tail-recursive enumeration 
                let rec toSeq stack = 
                    match stack with
                    | [] -> Seq.empty
                    | head :: tail ->
                        match head with
                        | Empty -> toSeq tail
                        | Cons (value, rest) -> seq { yield value; yield! toSeq <| rest () :: tail }
                        | Delay f -> toSeq <| f () :: tail
                        | Combine (first, second) -> toSeq <| first :: second :: tail
                (toSeq [self]).GetEnumerator() 
        interface IEnumerable with 
            member self.GetEnumerator() = (self :> IEnumerable<'T>).GetEnumerator() :> _ 

// Monoid Comprehension
type LazyListBuilder() = 
    member self.Yield value = Cons (value, fun () -> Empty)
    member self.YieldFrom value = value
    member self.Combine(first, second) = Combine (first, second) 
    member self.Delay f = Delay f
    member self.Zero() = Empty

let lazyList = new LazyListBuilder()


// Example
#time
type Tree<'T> = Empty | Branch of 'T * Tree<'T> * Tree<'T>

let rec createBalancedTree n = 
    if n <= 0 then Empty
    else Branch (n, createBalancedTree (n - 1), createBalancedTree (n - 1))

let rec createLeftSpinedTree n acc = 
    if n <= 0 then acc
    else createLeftSpinedTree (n - 1) (Branch (n, acc, Empty))

let tree = createBalancedTree 20
let leftSpinedTree = createLeftSpinedTree 100000 Empty

// Seq test
let rec flattenToSeq tree =
    match tree with
    | Empty -> Seq.empty
    | Branch (value, left, right) -> 
        seq { yield value; yield! flattenToSeq left; yield! flattenToSeq right }

tree
|> flattenToSeq
|> Seq.length // check time

leftSpinedTree
|> flattenToSeq
|> Seq.length // stack-overflow

// LazyList test
let rec flattenToLazyList tree =
    match tree with
    | Empty -> LazyList.Empty 
    | Branch (value, left, right) -> 
        lazyList { yield value; yield! flattenToLazyList left; yield! flattenToLazyList right }

tree
|> flattenToLazyList
|> Seq.length // check time

leftSpinedTree
|> flattenToLazyList
|> Seq.length // check time