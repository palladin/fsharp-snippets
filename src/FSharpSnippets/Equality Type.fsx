
// Equality without Leibniz's law
module Eq = 

    type Eq<'A, 'B> = private Refl of ('A -> 'B) * ('B -> 'A)
    
    let refl<'A> () : Eq<'A, 'A> = Refl (id, id)
    let sym : Eq<'A, 'B> -> Eq<'B, 'A> = fun (Refl (f, g)) -> Refl (g, f)
    let trans : Eq<'A, 'B> -> Eq<'B, 'C> -> Eq<'A, 'C> = 
        fun (Refl (f, g)) (Refl (h, k)) -> Refl (f >> h, k >> g)
    let cast : Eq<'A, 'B> -> 'A -> 'B = fun (Refl (f, _)) -> f

// Example: http://okmij.org/ftp/ML/GADT.ml
open Eq

type NodeLink = NodeLink
type NodeNoLink = NodeNoLink

type Node<'T> =
   | Text of string
   | Bold of Node<'T> list
   | Href of Eq<NodeLink, 'T> * string
   | Mref of Eq<NodeLink, 'T> * string * Node<NodeNoLink> list

let text txt = Text txt
let bold seq = Bold seq;;

let href lnk = Href (refl (), lnk)
let mref lnk seq = Mref (refl (), lnk, seq)

let test1 = bold [text "text1"; text "text2"]
let test2 = bold [text "text1"; href "link1"]

let test3 = mref "link2" [test1; test1]
let test4 = bold [text "text3"; test3]

// Type errors
// let test31 = mref "link2" [test1; test2]
// let test41 = mref "link3" [test1; test4]

let rec capnode<'T> : Node<'T> -> Node<'T> =  
 function 
   | Text x        -> Text (x.ToUpper())
   | Bold x        -> Bold (List.map capnode x)
   | Href (eq,x)   -> Href (eq, x.ToUpper())
   | Mref (eq,l,x) -> Mref (eq, l, List.map capnode x)


let test1c = capnode test1
let test3c = capnode test3
let test4c = capnode test4