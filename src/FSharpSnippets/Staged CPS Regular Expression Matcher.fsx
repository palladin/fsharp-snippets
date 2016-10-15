// Staged CPS Regular Expression Matcher
// Based on CPS RegEx matcher in http://dl.acm.org/citation.cfm?id=968582

#r "packages/FSharp.Compiler.Service.1.3.1.0/lib/net45/FSharp.Compiler.Service.dll"
#r "packages/QuotationCompiler.0.0.7-alpha/lib/net45/QuotationCompiler.dll"


open QuotationCompiler

open Microsoft.FSharp.Quotations

// <@ fun x -> (% <@ x @> ) @> ~ lambda (fun x -> x)
let lambda (f : Expr<'T> -> Expr<'R>) : Expr<'T -> 'R> =
    let var = new Var("__temp__", typeof<'T>)
    Expr.Cast<_>(Expr.Lambda(var,  f (Expr.Cast<_>(Expr.Var var))))


// <@ fun x y -> (% <@ x @> ... <@ y @> ) @> ~ lambda (fun x y -> x ... y )
let lambda2 (f : Expr<'T> -> Expr<'S> -> Expr<'R>) : Expr<'T -> 'S -> 'R> =
    let var = new Var("__temp'__", typeof<'T>)
    let var' = new Var("__temp''__", typeof<'S>)
    Expr.Cast<_>(Expr.Lambda(var, Expr.Lambda(var',  f (Expr.Cast<_>(Expr.Var var)) (Expr.Cast<_>(Expr.Var var')))))

type RegExp = 
    | Zero
    | One 
    | Char of char
    | Times of RegExp * RegExp
    | Plus of RegExp * RegExp
    | Star of RegExp


let rec matchRegExp (pattern : RegExp) (chars : Expr<char list>) (k : Expr<char list> -> Expr<bool>) : Expr<bool> = 
    match pattern with
    | Zero -> <@ false @>
    | One -> k chars
    | Char c -> <@ match %chars with x :: xs when x = c -> (% lambda (fun xs -> k xs)) xs | _ -> false @>
    | Times (l, r) -> matchRegExp l chars (fun chars -> matchRegExp r chars k)
    | Plus (l, r) -> <@ if (% matchRegExp l chars k) then true else (% matchRegExp r chars k) @>
    | Star exp -> 
        <@ let rec loop chars = 
            if (% lambda(fun chars -> k chars)) chars then true
            else (% lambda2(fun loop chars -> matchRegExp exp chars (fun chars -> <@ (%loop) %chars @>))) loop chars 
           loop %chars @>

let compileRegEx (pattern : RegExp) : string -> bool = 
    let f = QuotationCompiler.ToFunc(lambda (fun (chars : Expr<char list>) -> 
                                        matchRegExp pattern chars (fun chars -> <@ match %chars with [] -> true | _ -> false @>)))
    (fun text -> f () (List.ofSeq text))

// helpers
let char c = Char c
let (=>) l r = Times (l, r)
let (<|>) l r = Plus (l, r)
let (<*>) e = Star e
let (<+>) e = e => (<*>) e


// example c(a|d)+r
let pattern = char 'c' => (<+>) (char 'a' <|> char 'd') => char 'r'
let test = compileRegEx pattern
test "car" // true
test "cdr" // false
test "cr" // false
test "cddar" // true
test "cdda" // false

