// A line-by-line translation of Norvig's original Python code. An attempt to view F# as a "typed" Python.

// Norvig's Spelling Corrector: http://norvig.com/spell-correct.html
open System.IO open System.Text.RegularExpressions

let edits1 (word : string) = 
    let splits = [for i in 0 .. word.Length do yield (word.[0..i-1], word.[i..])]
    let deletes = [for a, b in splits do if b <> "" then yield a + b.[1..]]
    let transposes = [for a, b in splits do if b.Length > 1 then yield a + string b.[1] + string b.[0] + b.[2..]]
    let replaces = [for a, b in splits do for c in 'a'..'z' do if b <> "" then yield a + string c + b.[1..]]
    let inserts = [for a, b in splits do for c in 'a'..'z' do yield a + string c + b]
    deletes @ transposes @ replaces @ inserts |> Set.ofList

let NWORDS = 
    File.ReadAllText "big.txt" |> (Regex "[a-zA-Z]+").Matches |> Seq.cast 
    |> Seq.map (fun (m:Match) -> m.Value.ToLower()) |> Seq.countBy id |> Map.ofSeq

let known_edits2 word = [for e1 in edits1(word) do for e2 in edits1(e1) do if Map.containsKey e2 NWORDS then yield e2] |> Set.ofList
let known words = [for w in words do if Map.containsKey w NWORDS then yield w] |> Set.ofList

let (<||>) (first : Lazy<_>) (second : Lazy<_>) : Lazy<_> = lazy(if Set.isEmpty first.Value then second.Value else first.Value)
let correct word = 
    (lazy known([word]) <||> lazy known(edits1(word)) <||> lazy known_edits2(word) <||> lazy Set.singleton word).Value 
    |> Seq.sortBy (fun w -> -NWORDS.[w]) |> Seq.head

// Example
correct "speling"