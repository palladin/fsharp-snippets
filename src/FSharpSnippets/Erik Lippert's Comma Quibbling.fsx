// An old solution of mine to Erik Lippert's Comma Quibbling challenge. I really like this snippet because it shows that with F# we can hack elegant but also fast code.

// https://web.archive.org/web/20130202122134/http://blogs.msdn.com/b/ericlippert/archive/2009/04/15/comma-quibbling.aspx

#time
#r "packages/FSPowerPack.Core.Community.2.0.0.0/Lib/Net40/FSharp.PowerPack.dll"
open System
open System.Text


let format (words : seq<string>) =
    let sb (value : string) = new StringBuilder(value)
    let (<+>) (first : StringBuilder) (second : string) = first.Append(second)
    let rec format (words : LazyList<string>) acc =
        match words with
        | LazyList.Nil -> sb ""
        | LazyList.Cons(first, LazyList.Nil) -> sb first
        | LazyList.Cons(first, LazyList.Cons(second, LazyList.Nil)) -> acc <+> first <+> " and " <+> second
        | LazyList.Cons(first, rest) ->  acc <+> first <+> ", " |> format rest 
          
    let listOfWords = LazyList.ofSeq words  
    sprintf "{%s}" <| (format listOfWords (sb "")).ToString() 

["ABC"; "DEF"; "G"; "H" ] |> format
["ABC"; "DEF" ] |> format 
["ABC"] |> format
[] |> format
{1..10000} |> Seq.map string |> format 