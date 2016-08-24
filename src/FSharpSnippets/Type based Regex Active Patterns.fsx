// Regex match via Active Patterns with Type based value extraction.

// Based on http://blogs.msdn.com/b/chrsmith/archive/2008/02/22/regular-expressions-via-active-patterns.aspx

open System
open System.Text.RegularExpressions
  

let (|Match|_|) (pat:string) (inp:string) =
    let m = Regex.Match(inp, pat) in
    if m.Success
    then Some (List.tail [ for g in m.Groups -> g.Value ])
    else None

let convert<'ToType> value = Convert.ChangeType(value, typeof<'ToType>) :?> 'ToType
let (|Match3|_|) (pat:string) (inp:string) : ('T1 * 'T2 * 'T3) option =
    match (|Match|_|) pat inp with
    | Some (fst :: snd :: trd :: []) -> 
        try 
            Some (convert<'T1> fst, convert<'T2> snd, convert<'T3> trd) 
        with _ -> failwith "Match3 succeeded, but with type conversion errors"
    | Some [] -> failwith "Match3 succeeded, but no groups found. Use '(.*)' to capture groups"
    | Some _ -> failwith "Match3 succeeded, but did not find exactly three matches."
    | None -> None  

// Example
let (month, day, year) : (int * int * int) =
    match DateTime.Now.ToString() with
    | Match3 "(\d*)/(\d*)/(\d*).*" (a,b,c) -> (a,b,c)
    | _ -> failwith "Match Not Found."