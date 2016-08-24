// A crazy experiment for "Scalable" collections. Inspired by the paper "Fighting Bit Rot with Types"

// http://lampwww.epfl.ch/~odersky/papers/fsttcs2009.pdf
open System
open System.Text
open System.Collections.Generic

// Type classes encoding for iteration and building
type Iterable<'From, 'Elem> = Iterable of (('Elem -> unit) -> unit) 
type Iter = Iter with
    static member inline ($) (_ : Iter, values : 'Elem []) = 
        Iterable (fun f -> 
                    for value in values do
                        f value)
    static member inline ($) (_ : Iter, values : 'Elem list) = 
        Iterable (fun f -> 
                    for value in values do
                        f value)
    static member inline ($) (_ : Iter, values : Map<_, _>) = 
        Iterable (fun f -> 
                    for value in Map.toSeq values do
                        f value)
    static member inline ($) (_ : Iter, values : String) = 
        Iterable (fun f -> 
                    for value in values do
                        f value)

type CanBuildFrom<'From, 'Elem, 'To> = CanBuildFrom of ('From -> Builder<'Elem, 'To>)  
and Builder<'Elem, 'To> = Builder of ('Elem -> unit) * (unit -> 'To)
 
type CanBuild = CanBuild with

    static member inline (?<-) (_ : Map<_, _>, _ : CanBuild, _ : Map<'Key, 'Value>) = fun (_ : 'Key * 'Value) ->
        CanBuildFrom (fun _ ->
            let mapRef = ref Map.empty
            Builder ((fun (key, value) -> mapRef := Map.add key value !mapRef), fun () -> !mapRef))

    static member inline (?<-) (_ : Map<_, _>, _ : CanBuild, _ : 'Elem []) = fun (_ : 'Elem) ->
        CanBuildFrom (fun _ ->
            let list = new List<'Elem>()
            Builder ((fun elem -> list.Add elem), fun () -> list.ToArray()))

    static member inline (?<-) (_ : _ [], _ : CanBuild, _ : 'Elem []) = fun (_ : 'Elem) ->
        CanBuildFrom (fun _ ->
            let list = new List<'Elem>()
            Builder ((fun elem -> list.Add elem), fun () -> list.ToArray()))

    static member inline (?<-) (_ : _ list, _ : CanBuild, _ : 'Elem list) = fun (_ : 'Elem) ->
        CanBuildFrom (fun _ -> 
            let listRef = ref []
            Builder ((fun elem -> listRef := elem :: !listRef), fun () -> List.rev !listRef))
 
    static member inline (?<-) (_ : String, _ : CanBuild, _ : String) = fun (_ : Char) ->
        CanBuildFrom (fun _ ->
            let stringBuilder = new StringBuilder()
            Builder ((fun (elem : char) -> stringBuilder.Append elem |> ignore), fun () -> stringBuilder.ToString()))
 
    static member inline (?<-) (_ : String, _ : CanBuild, _ : 'Elem []) = fun (_ : 'Elem) ->
        CanBuildFrom (fun _ ->
            let list = new List<'Elem>()
            Builder ((fun elem -> list.Add elem), fun () -> list.ToArray()))
 
// Generic high-order functions
let inline map (f : ^A -> ^B) col : ^R =
    let (CanBuildFrom getBuilder) = (col ? (CanBuild) <- Unchecked.defaultof< ^R> ) Unchecked.defaultof< ^B>
    let (Builder (add, result)) = getBuilder col
    let (Iterable iter) = Iter $ col
    iter (fun x -> add (f x))
    result()

let inline filter (f : ^A -> bool) col : ^R =
    let (CanBuildFrom getBuilder) = (col ? (CanBuild) <- Unchecked.defaultof< ^R> ) Unchecked.defaultof< ^A>
    let (Builder (add, result)) = getBuilder col
    let (Iterable iter) = Iter $ col
    iter (fun x -> if f x then add x)
    result()
 
// Examples
let testMap = Map.add 1 2 Map.empty
let m : Map<int, int> = map (fun (x, y) -> (y, x)) testMap // map [(2, 1)]
let m' : int [] = map (fun (x, y) -> x + y) testMap // [|3|]
map (fun x -> x + 1) [|1..3|] // [|2; 3; 4|]
map (fun x -> x + 1) [1..3] // [2; 3; 4]
let s : String = map (fun c -> Char.ToUpper c) "abc" // "ABC"
map (fun c -> int c) "abc" // [|97; 98; 99|]

filter (fun x -> x > 2) [1..3] // [3]
let m'' : Map<int, int> = filter (fun (x, y) -> x = y) testMap // map []