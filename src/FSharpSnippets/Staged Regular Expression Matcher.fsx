// A staged a regular expression interpreter is a compiler!!!

// http://www.cs.princeton.edu/courses/archive/spr09/cos333/beautiful.html
// http://scala-lms.github.io/tutorials/regex.html


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
    let var = new Var("__temp__", typeof<'T>)
    let var' = new Var("__temp'__", typeof<'S>)
    Expr.Cast<_>(Expr.Lambda(var, Expr.Lambda(var',  f (Expr.Cast<_>(Expr.Var var)) (Expr.Cast<_>(Expr.Var var')))))

let rec matchsearch (regexp : string) (text : Expr<string>) : Expr<bool> = 
    if regexp.[0] = '^' then
        matchhere regexp 1 text <@ 0 @>
    else
    <@
        let text = %text
        let mutable start = -1
        let mutable found = false
        while not found && start < text.Length do
            start <- start + 1
            found <- (% lambda2(fun text start -> matchhere regexp 0 text start) ) text start
        found
    @>

and matchhere (regexp : string) (restart : int) 
              (text : Expr<string>) (start : Expr<int>) : Expr<bool> = 
    if restart = regexp.Length then
        <@ true @>
    else if regexp.[restart] = '$' && restart + 1 = regexp.Length then
        <@ %start = String.length %text @>
    else if restart + 1 < regexp.Length && regexp.[restart + 1] = '*' then
        matchstar regexp.[restart] regexp (restart + 2) text start
    else
    <@ 
        if %start < (%text).Length && (% matchchar regexp.[restart] <@ (%text).[%start] @> ) then
            (% matchhere regexp (restart + 1) text <@ %start + 1 @> )
        else false
    @>

and matchstar (c : char) (regexp : string) (restart : int) (text : Expr<string>) (start: Expr<int>) : Expr<bool> =
    <@
        let text = %text
        let mutable sstart = %start
        let mutable found = (% lambda2(fun text sstart -> matchhere regexp restart text sstart) ) text sstart
        let mutable failed = false
        while not failed && not found && sstart < text.Length do
          failed <- not ((% lambda2(fun (text : Expr<string>) (sstart : Expr<int>) -> matchchar c <@ (%text).[%sstart] @>) ) text sstart)
          sstart <- sstart + 1
          found <- (% lambda2(fun text sstart -> matchhere regexp restart text sstart) ) text sstart
    
        not failed && found
    @>

and matchchar (c: char) (t : Expr<char>) : Expr<bool> = 
    if c = '.' then <@ true @> 
    else <@ c = %t @>

let compileRegEx (pattern : string) : string -> bool = 
    let f = QuotationCompiler.ToFunc(lambda (fun text -> matchsearch pattern text))
    f ()

let testmatch (f : string -> bool) (text : string) (expected : bool) = 
    if f text <> expected then
        failwith "oups"

 // Examples

let ``^hello$`` = compileRegEx "^hello$"
let ``hell`` = compileRegEx "hell"
let ``hel*`` = compileRegEx "hel*"
let ``hel*$`` = compileRegEx "hel*$"
let ``ab`` = compileRegEx "ab"
let ``^ab`` = compileRegEx "^ab"
let ``a*b`` = compileRegEx "a*b"
let ``^ab*`` = compileRegEx "^ab*"
let ``^ab*$`` = compileRegEx "^ab*$"

testmatch ``^hello$`` "hello" true
testmatch ``^hello$`` "hell" false
testmatch ``hell`` "hello" true
testmatch ``hell`` "hell" true
testmatch ``hel*`` "he" true
testmatch ``hel*$`` "hello" false
testmatch ``hel*`` "yo hello" true
testmatch ``ab`` "hello ab hello" true
testmatch ``^ab``  "hello ab hello" false
testmatch ``a*b`` "hello aab hello" true
testmatch ``^ab*`` "abcd"  true
testmatch ``^ab*``  "a"  true
testmatch ``^ab*`` "ac" true
testmatch ``^ab*`` "bac" false
testmatch ``^ab*$`` "ac" false