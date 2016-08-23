// Functional style Regex engine

let char c (s : string) = seq { if s.Length > 0 && s.[0] = c then yield s.Substring(1) }

let (=>) l r s = seq { for sl in l s do for sr in r sl -> sr }

let (<|>) l r s = seq { yield! l s; yield! r s }

let rec (<*>) e s = seq { yield s; yield! (e => (<*>) e) s }

let (<+>) e = e => (<*>) e

// example c(a|d)+r
let pattern = char 'c' => (<+>) (char 'a' <|> char 'd') => char 'r'
