// Staged parser combinators

#r "packages/FSharp.Compiler.Service.1.3.1.0/lib/net45/FSharp.Compiler.Service.dll"
#r "packages/QuotationCompiler.0.0.7-alpha/lib/net45/QuotationCompiler.dll"


open QuotationCompiler

open Microsoft.FSharp.Quotations

let counter = ref 0

// <@ fun x -> (% <@ x @> ) @> ~ lambda (fun x -> x)
let lambda (f : Expr<'T> -> Expr<'R>) : Expr<'T -> 'R> =
    incr counter
    let var = new Var(sprintf "__paramTemp_%d__" !counter, typeof<'T>)
    Expr.Cast<_>(Expr.Lambda(var,  f (Expr.Cast<_>(Expr.Var var))))


// <@ fun x y -> (% <@ x @> ... <@ y @> ) @> ~ lambda (fun x y -> x ... y )
let lambda2 (f : Expr<'T> -> Expr<'S> -> Expr<'R>) : Expr<'T -> 'S -> 'R> =
    incr counter
    let var = new Var(sprintf "__paramTemp_%d__" !counter, typeof<'T>)
    incr counter
    let var' = new Var(sprintf "__paramTemp_%d__" !counter, typeof<'S>)
    Expr.Cast<_>(Expr.Lambda(var, Expr.Lambda(var',  f (Expr.Cast<_>(Expr.Var var)) (Expr.Cast<_>(Expr.Var var')))))

// <@ fun x y z -> (% <@ x @> ... <@ y @> ... <@ z @> ) @> ~ lambda (fun x y z -> x ... y ... z )
let lambda3 (f : Expr<'T> -> Expr<'S> -> Expr<'K> -> Expr<'R>) : Expr<'T -> 'S -> 'K -> 'R> =
    incr counter
    let var = new Var(sprintf "__paramTemp_%d__" !counter, typeof<'T>)
    incr counter
    let var' = new Var(sprintf "__paramTemp_%d__" !counter, typeof<'S>)
    incr counter
    let var'' = new Var(sprintf "__paramTemp_%d__" !counter, typeof<'K>)
    Expr.Cast<_>(Expr.Lambda(var, Expr.Lambda(var', Expr.Lambda(var'', f (Expr.Cast<_>(Expr.Var var)) (Expr.Cast<_>(Expr.Var var')) (Expr.Cast<_>(Expr.Var var''))))))


type Parser<'T> = Expr<string> -> Expr<int> -> (Expr<'T> -> Expr<string> -> Expr<int> -> Expr<bool>) -> Expr<bool>


// combinators
let pchar (matchChar : char) : Parser<char> = 
    fun str index k ->
        <@ let index = %index
           if (%str).Length = index then false 
           else 
            let current = (%str).[index]
            if current <> matchChar then 
                false
            else
                (% lambda2 (fun current index -> k current str index)) current (index + 1) @>


let (=>) (left : Parser<'T>) (right : Parser<'S>) : Parser<'T * 'S> = 
    fun str index k -> left str index (fun value str index -> right str index (fun value' str index -> k <@ (%value, %value') @> str index))

let (<|>) (left : Parser<'T>) (right : Parser<'T>) : Parser<'T> = 
    fun str index k -> <@ let test = (% left str index (fun value  str index -> k value str index)) 
                          if test then true
                          else (% right str index (fun value  str index -> k value str index))  
                       @>

let (<*>) (parser : Parser<'T>) : Parser<'T list> = 
    fun str index k -> 
        <@  let rec loop (index : int) (acc : 'T list) =
                let test = (% lambda3 (fun loop index acc -> parser str index (fun value str index -> <@ (%loop) %index (%value :: %acc) @>))) loop index acc
                if not test then
                    (% lambda2 (fun index acc -> k acc str index)) index acc
                else true
            loop %index []
        @>

let compileParser (parser : Parser<'T>) : string -> 'T option = 
    let f = QuotationCompiler.ToFunc(lambda (fun (str : Expr<string>) -> 
                                        <@ let resultRef = ref Unchecked.defaultof<'T>
                                           let test = (% lambda (fun resultRef -> parser str <@ 0 @>  (fun value _ _ -> <@ %resultRef := %value; true @>))) resultRef
                                           if test then Some !resultRef else None
                                        @>))
    (fun str -> f () str)

    

// Examples
let f = pchar 'a' |> compileParser
f "abc" // Some 'a'
f "bac" // None

let g = (pchar 'a' => pchar 'b') |> compileParser
g "abc" // Some ('a', 'b')
g "bac" // None

let h = (pchar 'a' <|> pchar 'b') |> compileParser
h "abc" // Some 'a'
h "bac" // Some 'b'
h "cab" // None

let k = ((<*>) (pchar 'a')) |> compileParser
k "abc" // Some ['a']
k "aabc" // Some ['a'; 'a']
k "bac" // None