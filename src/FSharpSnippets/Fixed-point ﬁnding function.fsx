// We start with an initial value and then applying f repeatedly, until the value does not change anymore.

let rec iterate f value = 
  seq { yield value; 
        yield! iterate f (f value) }

let fixedPoint f initial = 
    iterate f initial 
    |> Seq.pairwise 
    |> Seq.pick (fun (first, second) -> 
        if first = second then Some first else None)