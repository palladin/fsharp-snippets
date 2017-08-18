// Evens and Odds via Z3

#r "bin/Microsoft.Z3.dll"

open System
open System.IO
open System.Runtime.InteropServices
open System.Collections.Generic
open Microsoft.Z3



let ctx = new Context([|("model", "true")|] |> dict |> Dictionary)

// helpers
let True : BoolExpr = ctx.MkTrue()
let False : BoolExpr = ctx.MkFalse()
let Int : int -> IntNum = fun v -> ctx.MkInt(v)
let IntVar : string -> IntExpr = fun var -> ctx.MkIntConst(var) 
let FreshVar : unit -> IntExpr = fun () -> ctx.MkIntConst(Guid.NewGuid().ToString()) 
let Eq : Expr -> Expr -> BoolExpr = fun l r -> ctx.MkEq(l, r) 
let Mod : IntExpr -> IntExpr -> IntExpr = fun l r -> ctx.MkMod(l, r)
let Ite : BoolExpr -> Expr -> Expr -> Expr = fun p t e -> ctx.MkITE(p, t, e)
let Add : ArithExpr -> ArithExpr -> ArithExpr = fun l r -> ctx.MkAdd(l, r)
let Mul : ArithExpr -> ArithExpr -> ArithExpr = fun l r -> ctx.MkMul(l, r)
let And : BoolExpr[] -> BoolExpr = fun bools -> ctx.MkAnd(bools)
let Or : BoolExpr[] -> BoolExpr = fun bools -> ctx.MkOr(bools)


let rec compile : IntExpr list -> IntExpr -> IntExpr -> BoolExpr = 
    fun nums even odd ->
        match nums with
        | [] ->  And [|Eq even (Int 0); Eq odd (Int 0)|]
        | num :: nums -> 
            let even' = FreshVar ()
            let odd' = FreshVar ()
            let ite = 
                Ite (Eq (Mod num (Int 2)) (Int 0)) 
                                (And [|Eq even (Add even' (Int 1)); Eq odd odd'|])
                                (And [|Eq odd (Add odd' (Int 1)); Eq even even'|]) :?> BoolExpr
            And [|ite; compile nums even' odd'|]
        

let even = IntVar "even"
let odd = IntVar "odd"
let x = IntVar "x"
let twox = Mul (Int 2) x :?> IntExpr
let twoxplusone = Add (Mul (Int 2) x) (Int 1) :?> IntExpr
let testExpr = compile [Int 1; Int 2; Int 3;
                        twox; twoxplusone; 
                        Add twox twox :?> _; 
                        Add twoxplusone twoxplusone :?> _] even odd

let solver = ctx.MkSolver()
solver.Assert(testExpr)
let flag = solver.Check() = Status.SATISFIABLE

let model = solver.Model

string <| model.Evaluate(even) // 4
string <| model.Evaluate(odd) // 3









