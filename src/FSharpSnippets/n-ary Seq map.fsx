// A pattern for creating n-ary Seq.map functions.

let (<*>) fs xs = Seq.map2 (fun f x -> f x) fs xs

let map3 f xs bs cs = Seq.map2 f xs bs <*> cs
let map4 f xs bs cs ds = map3 f xs bs cs <*> ds

map4 (fun x b c d -> x + b + c + d) [1;2] [1;2] [1;2] [1;2] // [4; 8]