
// Type Safe Higher-order abstract syntax via GADT encoding

type Expr<'T> = 
    abstract Eval : unit -> 'T

let eval (x : Expr<'T>) = x.Eval()
let lift (value : 'T) = 
    {   new Expr<'T> with
            member self.Eval () = value 
    }

let tup (first : Expr<'T>) (second : Expr<'S>) = 
    {   new Expr<'T * 'S> with
            member self.Eval () = (eval first, eval second)
    }


let lam (f : Expr<'T> -> Expr<'R>) = 
    {   new Expr<'T -> 'R> with
            member self.Eval () = eval << f << lift 
    }

let app (f : Expr<'T -> 'R>) (x : Expr<'T>) = 
    {   new Expr<'R> with
            member self.Eval () = eval f <| eval x
    }

let rec fix (f : Expr<('T -> 'R) -> ('T -> 'R)>) =
    {   new Expr<'T -> 'R> with
            member self.Eval () = eval f <| (fun x -> eval (fix f) x)
    }

let fact = fix (lam (fun f -> lam (fun y -> lift (if eval y = 0 then 1 else eval y * (eval f) (eval y - 1)))))
    
eval fact 4
Copy linkCopy sourceRaw viewLoad in New version