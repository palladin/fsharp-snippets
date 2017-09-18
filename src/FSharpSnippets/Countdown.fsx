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
    let Int : int -> uint32 -> BitVecNum = fun v w -> ctx.MkBV(v, w)
    let IntVar : string -> uint32 -> BitVecExpr = fun var w -> ctx.MkBVConst(var, w) 
    let FreshVar : unit -> uint32 -> BitVecExpr = fun () w -> ctx.MkBVConst(Guid.NewGuid().ToString(), w) 
    let Eq : Expr -> Expr -> BoolExpr = fun l r -> ctx.MkEq(l, r)
    let Gt : BitVecExpr -> BitVecExpr -> BoolExpr = fun l r -> ctx.MkBVSGT(l, r) 
    let Le : BitVecExpr -> BitVecExpr -> BoolExpr = fun l r -> ctx.MkBVSLE(l, r) 
    let Ite : BoolExpr -> Expr -> Expr -> Expr = fun p t e -> ctx.MkITE(p, t, e)
    let Add : BitVecExpr -> BitVecExpr -> BitVecExpr = fun l r -> ctx.MkBVAdd(l, r)
    let Sub : BitVecExpr -> BitVecExpr -> BitVecExpr = fun l r -> ctx.MkBVSub(l, r)
    let Mul : BitVecExpr -> BitVecExpr -> BitVecExpr = fun l r -> ctx.MkBVMul(l, r)
    let Div : BitVecExpr -> BitVecExpr -> BitVecExpr = fun l r -> ctx.MkBVSDiv(l, r)
    let Mod : BitVecExpr -> BitVecExpr -> BitVecExpr = fun l r -> ctx.MkBVSMod(l, r)
    let And : BoolExpr[] -> BoolExpr = fun bools -> ctx.MkAnd(bools)
    let Or : BoolExpr[] -> BoolExpr = fun bools -> ctx.MkOr(bools)
    let Not : BoolExpr -> BoolExpr = fun bool -> ctx.MkNot(bool)
    let ArrayVar : string -> uint32 -> ArrayExpr = fun v w -> ctx.MkConst(v, ctx.MkArraySort(ctx.MkBitVecSort w, ctx.MkBitVecSort w)) :?> _
    let Get : ArrayExpr -> BitVecExpr -> BitVecExpr = fun array index -> ctx.MkSelect(array, index) :?> _
    let Set : ArrayExpr -> BitVecExpr -> BitVecExpr -> ArrayExpr = fun array index v -> ctx.MkStore(array, index, v)
    let distinct : Expr[] -> BoolExpr = fun exprs -> ctx.MkDistinct(exprs)

    let w = 16u
    let eval : (BitVecExpr * BitVecExpr) list -> BitVecExpr -> BoolExpr = fun ops target -> 
        let rec eval' : (BitVecExpr * BitVecExpr) list -> BitVecExpr -> ArrayExpr -> BoolExpr = 
            fun ops sp st ->
                match ops with
                | [] -> And [|Eq (Get st sp) target; Eq sp (Int 1 w) |]
                | (op, opr) :: ops -> 
                    let sp' = FreshVar () w
                    let validOp = 
                        Ite (Eq op (Int 0 w)) 
                            (Eq sp' (Add sp (Int 1 w))) 
                            (Ite (Eq op (Int 1 w)) 
                                (Eq sp' (Sub sp (Int 1 w))) 
                                False) :?> BoolExpr
                    let validAdd = 
                        Ite (And [|(Eq op (Int 1 w)); (Eq opr (Int 0 w))|])
                             (Le (Get st (Sub sp (Int 1 w))) (Get st sp))
                             True :?> BoolExpr
                    let validSub = 
                        Ite (And [|(Eq op (Int 1 w)); (Eq opr (Int 1 w))|])
                             (Gt (Get st (Sub sp (Int 1 w))) (Get st sp))
                             True :?> BoolExpr
                    let validMul = 
                        Ite (And [|(Eq op (Int 1 w)); (Eq opr (Int 2 w))|])
                             (And [|Not (Eq (Get st (Sub sp (Int 1 w))) (Int 1 w));
                                    Not (Eq (Get st sp) (Int 1 w));
                                    Le (Get st (Sub sp (Int 1 w))) (Get st sp) |])
                             True :?> BoolExpr
                    let validDiv = 
                        Ite (And [|(Eq op (Int 1 w)); (Eq opr (Int 3 w))|])
                             (And [|(Eq (Mod ((Get st (Sub sp (Int 1 w))) :?> _) ((Get st sp) :?> _)) (Int 0 w)); 
                                    (Not (Eq (Get st sp) (Int 1 w)));
                                    (Not (Eq (Get st sp) (Int 0 w)));|])
                             True :?> BoolExpr
                                                 
                    let st' = 
                        Ite (Eq op (Int 0 w))
                            (Set st sp' opr)
                            (Ite (Eq op (Int 1 w))
                                        (Ite (Eq opr (Int 0 w)) 
                                            (Set st sp' (Add (Get st (Sub sp (Int 1 w))) (Get st sp))) 
                                            (Ite (Eq opr (Int 1 w)) 
                                                (Set st sp' (Sub (Get st (Sub sp (Int 1 w))) (Get st sp))) 
                                                (Ite (Eq opr (Int 2 w)) 
                                                    (Set st sp' (Mul (Get st (Sub sp (Int 1 w))) (Get st sp))) 
                                                    (Ite (Eq opr (Int 3 w)) 
                                                        (Set st sp' (Div (Get st (Sub sp (Int 1 w))) (Get st sp)))  
                                                        st))))
                                     
                                    st) :?> ArrayExpr
                    And [|validOp; validAdd; validSub; validMul; validDiv; (Gt sp' (Int 0 w)); eval' ops sp' st'|]

        let sp = IntVar "sp" w
        let st = ArrayVar "st" w
        And [|(Eq sp (Int 0 w)); eval' ops sp st;|]

    let rec distinctOperants : (BitVecExpr * BitVecExpr) list -> Expr list -> BoolExpr =
        fun ops oprs ->
            match ops with
            | [] -> oprs |> List.toArray |> distinct
            | (op, opr) :: ops -> 
                let opr' = FreshVar () w
                let ite =
                    Ite (Eq op (Int 0 w))
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
            let ops = [ for i in {0..numOfInstrs - 1} -> (IntVar (sprintf "op-%d" i) w, IntVar (sprintf "opr-%d" i) w)]
            let validEval = eval ops (Int target w)
            let validRanges = 
                And 
                    [| for (op, opr) in ops do
                            yield Ite (Eq op (Int 0 w)) 
                                    (Or [| for num in nums -> Eq opr (Int num w) |])
                                    (Or [| for i in {0..3} -> Eq opr (Int i w)|]) :?> BoolExpr
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

    





