// A simple yet powerful library for parallel collection processing. Inspired by Clojure's Reducers.

// http://clojure.com/blog/2012/05/08/reducers-a-library-and-model-for-collection-processing.html


#r "packages/FSPowerPack.Parallel.Seq.Community.2.0.0.0/Lib/Net40/FSharp.PowerPack.Parallel.Seq.dll"

module Reducer =
    open System 
    open System.Text
    open System.Collections.Generic
    open System.Linq
    
    type ReduceFunc<'T,'R> =
        abstract Invoke : 'T * 'R -> 'R

    type CombineFunc<'R> =
        abstract Invoke : 'R * 'R -> 'R    

    type Reducer<'T> = 
        abstract Apply<'R> : ReduceFunc<'T, 'R> * CombineFunc<'R> * (unit -> 'R) -> 'R 
        
    // helper functions
    let inline toReduceFunc (f : 'T -> 'R -> 'R) =
        { 
            new ReduceFunc<'T, 'R> with 
                member self.Invoke(value : 'T, acc : 'R) = f value acc 
        }
        
    let inline toCombineFunc (f : 'R -> 'R -> 'R) =
        { 
            new CombineFunc<'R> with 
                member self.Invoke(left : 'R, right : 'R) = f left right 
        }

    // executor functions
    let inline toSeqReducer (values : seq<'T>) : Reducer<'T> =
        { 
            new Reducer<'T> with 
                member self.Apply<'R>(rf : ReduceFunc<'T, 'R>, _, init : unit -> 'R) : 'R =  
                    let mutable r = init ()
                    for value in values do
                        r <- rf.Invoke(value, r) 
                    r
        }
        
    let inline toParallelReducer (seqReduceCount : int) (values : 'T []) : Reducer<'T> =
        { 
            new Reducer<'T> with 
                member self.Apply<'R>(rf : ReduceFunc<'T, 'R>, cf : CombineFunc<'R>, init : unit -> 'R) : 'R =  
                    let rec reduceCombine s e =
                        async { 
                            if e - s <= seqReduceCount then
                                let s' = if s > 0 then s + 1 else s
                                let result = 
                                    let mutable r = init()
                                    for i = s' to e do
                                        r <- rf.Invoke(values.[i], r) 
                                    r
                                return result
                            else 
                                let m = (s + e) / 2
                                let! result =  Async.Parallel [| reduceCombine s m; reduceCombine m e |]
                                return cf.Invoke(result.[0], result.[1])
                        }
                    reduceCombine 0 (values.Length - 1) |> Async.RunSynchronously
        }

    let inline toParallelLinqReducer (values : seq<'T>) : Reducer<'T> =
        { 
            new Reducer<'T> with 
                member self.Apply<'R>(rf : ReduceFunc<'T, 'R>, cf : CombineFunc<'R>, init : unit -> 'R) : 'R =  
                    ParallelEnumerable.Aggregate(values.AsParallel(), Func<'R>(init), 
                                                    Func<'R, 'T, 'R>(fun acc v -> rf.Invoke(v, acc)),
                                                    Func<'R, 'R, 'R>(fun left right -> cf.Invoke(left, right)), Func<'R, 'R>(id))
        }

    // transform functions
    let inline collect (f : 'A -> Reducer<'B>) (input : Reducer<'A>) : Reducer<'B> = 
            { 
            new Reducer<'B> with 
            member self.Apply<'R> (rf, cf, init) = 
                    input.Apply<'R>(toReduceFunc (fun a r -> (f a).Apply(rf, cf, fun () -> r)), cf, init)
            }        
                    
    let inline map (f : 'A -> 'B) (input : Reducer<'A>) : Reducer<'B> =
        { 
            new Reducer<'B> with 
            member self.Apply<'R> (rf, cf, init) = 
                    input.Apply<'R>(toReduceFunc (fun a r -> rf.Invoke(f a, r)), cf, init)
        }

    let inline filter (p : 'A -> bool) (input : Reducer<'A>) : Reducer<'A> =
        { 
            new Reducer<'A> with 
                member self.Apply<'R> (rf, cf, init) = 
                    input.Apply<'R>(toReduceFunc (fun a r -> if p a then rf.Invoke(a, r) else r), cf, init)
        }

    // reduce functions
    let inline reduce (reducef : 'T -> 'R -> 'R) (combineF : 'R -> 'R -> 'R) (init : (unit -> 'R)) (reducer : Reducer<'T>) : 'R = 
        reducer.Apply(toReduceFunc reducef, toCombineFunc combineF, init)

    let inline sum (reducer : Reducer<int>) : int = 
        reduce (+) (+) (fun () -> 0) reducer

    let inline length (reducer : Reducer<'T>) : int =
        reduce (fun _ r -> r + 1) (+) (fun () -> 0) reducer

    let inline concat (reducer : Reducer<string>) : string = 
        let result = 
            reduce (fun (v : string) (builder : StringBuilder) -> builder.Append(v))
                    (fun (left : StringBuilder) (right : StringBuilder) -> left.Append(right)) 
                    (fun () -> new StringBuilder())
                    reducer
        result.ToString()

    let inline toArray (reducer : Reducer<'T>) : 'T [] =
        let result = 
            reduce (fun v (list : List<'T>) -> list.Add(v); list)
                    (fun (left : List<'T>) (right : List<'T>) -> left.AddRange(right); left) 
                    (fun () -> new List<'T>())
                    reducer 
        result.ToArray()

    let inline groupBy (selectorF : 'T -> 'Key) 
                        (transformF : 'T -> 'Elem) 
                        (aggregateF : 'Key * seq<'Elem> -> 'Elem) 
                        (reducer : Reducer<'T>) : seq<'Key * 'Elem> =
        let inline reduceF (v : 'T) (r : Dictionary<'Key, List<'Elem>>) =
            let key = selectorF v
            let elem = transformF v
            if r.ContainsKey(key) then
                r.[key].Add(elem)
                let result = (key, r.[key]) |> aggregateF
                r.[key].Clear()
                r.[key].Add(result)
            else 
                r.Add(key, new List<_>([| elem |]))
            r
        let inline combineF (left : Dictionary<'Key, List<'Elem>>) (right : Dictionary<'Key, List<'Elem>>) =
            for keyValue in right do
                if left.ContainsKey(keyValue.Key) then
                    left.[keyValue.Key].AddRange(right.[keyValue.Key])
                    let result = (keyValue.Key, left.[keyValue.Key]) |> aggregateF
                    left.[keyValue.Key].Clear()
                    left.[keyValue.Key].Add(result)
                else
                    left.[keyValue.Key] <- new List<_>([| (keyValue.Key, keyValue.Value) |> aggregateF |])
            left
                    
        let result = 
            reduce  reduceF combineF
                    (fun () -> new Dictionary<'Key, List<'Elem>>())
                    reducer
        result |> Seq.map (fun keyValue -> (keyValue.Key, (keyValue.Key, keyValue.Value) |> aggregateF))

    let inline countBy (selectorF : 'T -> 'Key) (reducer : Reducer<'T>) : seq<'Key * int> =
        let inline reduceF (v : 'T) (r : Dictionary<'Key, int>) =
            let key = selectorF v
            if r.ContainsKey(key) then
                r.[key] <- r.[key] + 1
            else 
                r.[key] <- 1
            r
        let inline combineF (left : Dictionary<'Key, int>) (right : Dictionary<'Key, int>) =
            for keyValue in right do
                if left.ContainsKey(keyValue.Key) then
                    left.[keyValue.Key] <- left.[keyValue.Key] + right.[keyValue.Key]
                else
                    left.[keyValue.Key] <- keyValue.Value
            left
                    
        let result = 
            reduce  reduceF combineF
                    (fun () -> new Dictionary<'Key, int>())
                    reducer
        result |> Seq.map (fun keyValue -> (keyValue.Key, keyValue.Value))

// Example - wordcount

let lines = System.IO.File.ReadAllLines("largefile.txt")

lines
|> Reducer.toParallelReducer 10
|> Reducer.collect (fun line -> Reducer.toSeqReducer <| line.Split(' '))
|> Reducer.countBy id
// |> Reducer.groupBy id (fun _ -> 1) (fun (_, items) -> Seq.sum items)