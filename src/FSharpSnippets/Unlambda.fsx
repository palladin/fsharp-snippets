// A minimal interpreter for David Madore's crazy-esoteric programming language.

// http://www.madore.org/~david/programs/unlambda/

type C = F -> unit
and F = F of ((F * C) -> unit)

let getReader (source : string) = 
    let pos = ref 0
    (fun () -> let result = source.[!pos] in pos := !pos + 1; result)

let eval (source : string) =
    let apply (first : C -> unit) (second : C -> unit) : C -> unit = 
        fun c -> first (fun (F f) -> second (fun g -> f(g, c)))
    let rec eval' reader : C -> unit =
        let c = reader() 
        match c with
        | '`' -> 
            let first, second = eval' reader, eval' reader
            apply first second
        | 's' -> 
            fun c -> c <| F (fun (x, c') ->  c' <| F (fun (y, c'') -> c'' <| F (fun (z, c''') ->  
                apply (apply (fun c -> c x) (fun c -> c z)) (apply (fun c -> c y) (fun c -> c z)) <| c'''))) 
        | 'k' -> fun c -> c <| F (fun (x, c') -> c' <| F (fun (_, c'') -> c'' x))
        | 'i' -> fun c -> c <| F (fun (x, c') -> c' x)
        | 'c' -> fun c -> c <| F (fun (F f, c') -> f (F (fun (x, _) -> c' x), c') )
        | '.' -> 
            let char = reader()
            fun c -> c <| F (fun (x, c') -> printf "%c" char; c' x)
        | 'r' -> fun c -> c <| F (fun (x, c') -> printf "\n"; c' x)
        | _ -> failwithf "Invalid symbol %c" c 
    let c = eval' <| getReader source
    c (fun _ -> ())

// Examples
eval "```si`k``s.H``s.e``s.l``s.l``s.o``s. ``s.w``s.o``s.r``s.l``s.d``s.!``sri``si``si``si``si``si``si``si``si`ki" // Hello World! * 8
eval "```s``s``sii`ki`k.*``s``s`ks``s`k`s`ks``s``s`ks``s`k`s`kr``s`k`sikk`k``s`ksk" // factorial
eval "``cir" // call/cc