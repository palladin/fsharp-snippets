// A CPS version of FuncList, in order to avoid blowing the stack.

// Continuation of http://fssnip.net/5i

type FuncList<'a> = 'a list -> 'a list
type CPSFuncList<'a> = FuncList<'a> -> FuncList<'a>
// Monoid comprehension
type CPSFuncListBuilder() =
    member self.Combine (first : CPSFuncList<'a>, second : CPSFuncList<'a>) : CPSFuncList<'a>  = 
        (fun k -> second (fun tail -> first (fun tail' -> k tail') tail))
    member self.Zero() : CPSFuncList<'a> = (fun k tail -> k tail)
    member self.Yield (value : 'a) : CPSFuncList<'a> = (fun k tail -> k (value :: tail))
    member self.YieldFrom (value : CPSFuncList<'a>) : CPSFuncList<'a> = value
    member self.Delay ( f : unit -> CPSFuncList<'a>) : CPSFuncList<'a> = (fun k tail -> f () k tail)
 
let cpsFuncList = new CPSFuncListBuilder()


// examples
let rec reverse list =
    match list with
    | [] -> cpsFuncList.Zero()
    | x :: xs -> cpsFuncList { yield! reverse xs; yield x }

let rec map f list =
    match list with
    | [] -> cpsFuncList.Zero()
    | x :: xs -> cpsFuncList { yield f x; yield! map f xs }

reverse [1..1000000] id []
map ((+) 1) [1..1000000] id []
Copy linkCopy sourceRaw viewLoad in New version