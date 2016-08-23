
// A FuncList is a "list-like" datatype with constant time append (represented as a function of cons-lists). The implementation is based on a convenient computation builder.

// for more info http://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/lists.pdf

type FuncList<'a> = 'a list -> 'a list
// Monoid comprehension
type FuncListBuilder() =
    member self.Combine (first : FuncList<'a>, second : FuncList<'a>) : FuncList<'a>  = (first << second) 
    member self.Zero() : FuncList<'a> = id
    member self.Yield (value : 'a) : FuncList<'a> = fun tail -> value :: tail
    member self.YieldFrom (value : FuncList<'a>) : FuncList<'a> = value
    member self.Delay ( f : unit -> FuncList<'a>) : FuncList<'a> = (fun tail -> f () tail)
 
let funcList = new FuncListBuilder()


// example
let rec reverse list =
    match list with
    | [] -> funcList.Zero()
    | x :: xs -> funcList { yield! reverse xs; yield x }

reverse [1..10] [] // returns [10; 9; 8; 7; 6; 5; 4; 3; 2; 1]