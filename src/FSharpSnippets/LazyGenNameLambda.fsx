
// Based on "Using Circular Programs for Higher-Order Syntax" https://emilaxelsson.github.io/documents/axelsson2013using.pdf

let force : Lazy<'T> -> 'T = fun x -> x.Force()

type LazyName = Lazy<int>
type Name = int

type Exp<'T> =
    | Var of 'T
    | App of Exp<'T> * Exp<'T>
    | Lam of 'T * Exp<'T>


let rec maxBV : Exp<LazyName> -> Name = fun e ->
    match e with
    | Var _ -> 0
    | App (f, e) -> max (maxBV f) (maxBV e)
    | Lam (n, _) -> force n

let lam : (Exp<LazyName> -> Exp<LazyName>) -> Exp<LazyName> = fun f ->
    let rec result : Exp<LazyName> * LazyName = (f (Var (lazy (force (snd result)))), (lazy (maxBV (fst result) + 1)))
    Lam (snd result, fst result)

let rec forceExp : Exp<LazyName> -> Exp<Name> = fun e ->
    match e with
    | Var n -> Var (force n)
    | App (f, e) -> App (forceExp f, forceExp e)
    | Lam (n, e) -> Lam (force n, forceExp e)


let test = lam (fun x1 -> lam (fun x2 -> lam (fun x3 -> App (App(x1, x2), x3))))

forceExp <| test


