// Countdown problem via Z3

#r "bin/Microsoft.Z3.dll"

open System
open System.IO
open System.Runtime.InteropServices
open System.Collections.Generic
open Microsoft.Z3

module Solver = 
    let ctx = new Context([|("model", "true")|] |> dict |> Dictionary)

    // helpers
    let True : BoolExpr = ctx.MkTrue()
    let False : BoolExpr = ctx.MkFalse()
    let Int : int -> IntNum = fun v -> ctx.MkInt(v)
    let IntVar : string -> IntExpr = fun var -> ctx.MkIntConst(var) 
    let FreshVar : unit -> IntExpr = fun () -> ctx.MkIntConst(Guid.NewGuid().ToString()) 
    let Eq : Expr -> Expr -> BoolExpr = fun l r -> ctx.MkEq(l, r)
    let Gt : ArithExpr -> ArithExpr -> BoolExpr = fun l r -> ctx.MkGt(l, r) 
    let Le : ArithExpr -> ArithExpr -> BoolExpr = fun l r -> ctx.MkLe(l, r) 
    let Ite : BoolExpr -> Expr -> Expr -> Expr = fun p t e -> ctx.MkITE(p, t, e)
    let Add : ArithExpr -> ArithExpr -> ArithExpr = fun l r -> ctx.MkAdd(l, r)
    let Sub : ArithExpr -> ArithExpr -> ArithExpr = fun l r -> ctx.MkSub(l, r)
    let Mul : ArithExpr -> ArithExpr -> ArithExpr = fun l r -> ctx.MkMul(l, r)
    let Div : ArithExpr -> ArithExpr -> ArithExpr = fun l r -> ctx.MkDiv(l, r)
    let Mod : IntExpr -> IntExpr -> IntExpr = fun l r -> ctx.MkMod(l, r)
    let And : BoolExpr[] -> BoolExpr = fun bools -> ctx.MkAnd(bools)
    let Or : BoolExpr[] -> BoolExpr = fun bools -> ctx.MkOr(bools)
    let Not : BoolExpr -> BoolExpr = fun bool -> ctx.MkNot(bool)
    let ArrayVar : string -> ArrayExpr = fun v -> ctx.MkConst(v, ctx.MkArraySort(ctx.IntSort, ctx.IntSort)) :?> _
    let Get : ArrayExpr -> ArithExpr -> ArithExpr = fun array index -> ctx.MkSelect(array, index) :?> _
    let Set : ArrayExpr -> ArithExpr -> ArithExpr -> ArrayExpr = fun array index v -> ctx.MkStore(array, index, v)
    let distinct : Expr[] -> BoolExpr = fun exprs -> ctx.MkDistinct(exprs)


    let eval : (IntExpr * IntExpr) list -> IntExpr -> BoolExpr = fun ops target -> 
        let rec eval' : (IntExpr * IntExpr) list -> IntExpr -> ArrayExpr -> BoolExpr = 
            fun ops sp st ->
                match ops with
                | [] -> And [|Eq (Get st sp) target; Eq sp (Int 1) |]
                | (op, opr) :: ops -> 
                    let sp' = FreshVar ()
                    let validOp = 
                        Ite (Eq op (Int 0)) 
                            (Eq sp' (Add sp (Int 1))) 
                            (Ite (Eq op (Int 1)) 
                                (Eq sp' (Sub sp (Int 1))) 
                                False) :?> BoolExpr
                    let validAdd = 
                        Ite (And [|(Eq op (Int 1)); (Eq opr (Int 0))|])
                             (Le (Get st (Sub sp (Int 1))) (Get st sp))
                             True :?> BoolExpr
                    let validSub = 
                        Ite (And [|(Eq op (Int 1)); (Eq opr (Int 1))|])
                             (Gt (Get st (Sub sp (Int 1))) (Get st sp))
                             True :?> BoolExpr
                    let validMul = 
                        Ite (And [|(Eq op (Int 1)); (Eq opr (Int 2))|])
                             (And [|Not (Eq (Get st (Sub sp (Int 1))) (Int 1));
                                    Not (Eq (Get st sp) (Int 1));
                                    Le (Get st (Sub sp (Int 1))) (Get st sp) |])
                             True :?> BoolExpr
                    let validDiv = 
                        Ite (And [|(Eq op (Int 1)); (Eq opr (Int 3))|])
                             (And [|(Eq (Mod ((Get st (Sub sp (Int 1))) :?> _) ((Get st sp) :?> _)) (Int 0)); 
                                    (Not (Eq (Get st sp) (Int 1)));
                                    (Not (Eq (Get st sp) (Int 0)));|])
                             True :?> BoolExpr
                                                 
                    let st' = 
                        Ite (Eq op (Int 0))
                            (Set st sp' opr)
                            (Ite (Eq op (Int 1))
                                        (Ite (Eq opr (Int 0)) 
                                            (Set st sp' (Add (Get st (Sub sp (Int 1))) (Get st sp))) 
                                            (Ite (Eq opr (Int 1)) 
                                                (Set st sp' (Sub (Get st (Sub sp (Int 1))) (Get st sp))) 
                                                (Ite (Eq opr (Int 2)) 
                                                    (Set st sp' (Mul (Get st (Sub sp (Int 1))) (Get st sp))) 
                                                    (Ite (Eq opr (Int 3)) 
                                                        (Set st sp' (Div (Get st (Sub sp (Int 1))) (Get st sp)))  
                                                        st))))
                                     
                                    st) :?> ArrayExpr
                    And [|validOp; validAdd; validSub; validMul; validDiv; (Gt sp' (Int 0)); eval' ops sp' st'|]

        let sp = IntVar "sp"
        let st = ArrayVar "st"
        And [|(Eq sp (Int 0)); eval' ops sp st;|]

    let rec distinctOperants : (IntExpr * IntExpr) list -> Expr list -> BoolExpr =
        fun ops oprs ->
            match ops with
            | [] -> oprs |> List.toArray |> distinct
            | (op, opr) :: ops -> 
                let opr' = FreshVar ()
                let ite =
                    Ite (Eq op (Int 0))
                        (Eq opr opr')
                        True :?> BoolExpr
                And [|ite; distinctOperants ops ((opr' :> _) :: oprs)|]

    let rec rpnEval : (int * int) list -> int list -> int list =
        fun ops st ->
            match ops with
            | [] -> st 
            | (op, opr) :: ops -> 
                match op with
                | 0 -> rpnEval ops (opr :: st)
                | 1 -> 
                    match st with
                    | x :: y :: st' ->
                        match opr with
                        | 0 -> rpnEval ops ((y + x) :: st')
                        | 1 -> rpnEval ops ((y - x) :: st')
                        | 2 -> rpnEval ops ((y * x) :: st')
                        | 3 -> rpnEval ops ((y / x) :: st')
                        | _ -> failwith "oups"
                    | _ -> failwith "oups"
                | _ -> failwith "oups"


    let solve : int list -> int -> int -> (int * int) list option = 
        fun nums target numOfInstrs ->
            let ops = [ for i in {0..numOfInstrs - 1} -> (IntVar (sprintf "op-%d" i), IntVar (sprintf "opr-%d" i)) ]
            let validEval = eval ops (Int target)
            let validRanges = 
                And 
                    [| for (op, opr) in ops do
                            yield Ite (Eq op (Int 0)) 
                                    (Or [| for num in nums -> Eq opr (Int num) |])
                                    (Or [| for i in {0..3} -> Eq opr (Int i)|]) :?> BoolExpr
                    |]

            let validDistinctOprs = distinctOperants ops []
            let solver = ctx.MkSolver()
            solver.Assert(And [|validRanges; validDistinctOprs; validEval|])
            if solver.Check() = Status.SATISFIABLE then
                let model = solver.Model
                let ops' = [ for (op, opr) in ops -> (Int32.Parse(string (model.Evaluate(op)))), (Int32.Parse (string (model.Evaluate(opr)))) ]
                if rpnEval ops' [] = [target] then
                    Some ops'
                else failwith "oups"
            else None
            

let nums = [1; 3; 7; 10; 25; 50]
let target = 765

Solver.solve nums target 7

    





