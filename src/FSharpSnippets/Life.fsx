// Life patterns via Z3
// Inspired by https://www.youtube.com/watch?v=g4lhrVPDUG0

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
let Int : int -> uint32 -> BitVecNum = fun v w -> ctx.MkBV(v, w)
let IntVar : string -> uint32 -> BitVecExpr = fun var w -> ctx.MkBVConst(var, w) 
let FreshVar : uint32 -> BitVecExpr = fun w -> ctx.MkBVConst(Guid.NewGuid().ToString(), w) 
let Eq : Expr -> Expr -> BoolExpr = fun l r -> ctx.MkEq(l, r) 
let Ite : BoolExpr -> Expr -> Expr -> Expr = fun p t e -> ctx.MkITE(p, t, e)
let And : BoolExpr[] -> BoolExpr = fun bools -> ctx.MkAnd(bools)
let Or : BoolExpr[] -> BoolExpr = fun bools -> ctx.MkOr(bools)
let Not : BoolExpr -> BoolExpr = fun bool -> ctx.MkNot(bool)
let Add : BitVecExpr -> BitVecExpr -> BitVecExpr = fun l r -> ctx.MkBVAdd(l, r)

let width = 15
let height = 7

let initBoard : Expr[][] = 
    [| for i in {0..height - 1} ->
            [| for j in {0..width - 1} -> IntVar (sprintf "X_%d_%d" i j) 1u :> _ |] |]

let finalBoard : Expr[][] = 
    [| for i in {0..height - 1} ->
            [| for j in {0..width - 1} -> IntVar (sprintf "Υ_%d_%d" i j) 1u :> _ |] |]

let pattern : int[][] = 
    [|[|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|];
      [|0; 1; 0; 0; 0; 1; 0; 1; 1; 1; 0; 1; 1; 1; 0|];
      [|0; 1; 0; 0; 0; 1; 0; 1; 0; 0; 0; 1; 0; 0; 0|]
      [|0; 1; 0; 0; 0; 1; 0; 1; 1; 0; 0; 1; 1; 0; 0|]
      [|0; 1; 0; 0; 0; 1; 0; 1; 0; 0; 0; 1; 0; 0; 0|]
      [|0; 1; 1; 1; 0; 1; 0; 1; 0; 0; 0; 1; 1; 1; 0|]
      [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]|]

let validValues = initBoard |> Array.collect id |> Array.map (fun x -> Or [|Eq x (Int 0 1u); Eq x (Int 1 1u)|] ) |> And
let validFinalPattern = 
    And [| for i in {0..height - 1} ->
            And [| for j in {0..width - 1} -> Eq finalBoard.[i].[j] (Int pattern.[i].[j] 1u) |] |]

let getNeighborhoods : Expr[][] -> int -> int -> Expr list = fun board i j ->
    match i, j with
    | 0, 0 -> [board.[0].[1]; board.[1].[0]; board.[1].[1] ]
    | i, j when i = height - 1 && j = width - 1 -> 
        [board.[i].[j - 1]; board.[i - 1].[j]; board.[i - 1].[j - 1] ] 
    | 0, j when j = width - 1 -> 
        [board.[0].[j - 1]; board.[1].[j - 1]; board.[1].[j] ]
    | i, 0 when i = height - 1 -> 
        [board.[i - 1].[0]; board.[i - 1].[1]; board.[i].[1] ]
    | 0, j -> 
        [board.[0].[j - 1]; board.[0].[j + 1]; board.[1].[j + 1]; board.[1].[j - 1]; board.[1].[j] ]
    | i, 0 -> 
        [board.[i - 1].[0]; board.[i + 1].[0]; board.[i + 1].[1]; board.[i - 1].[1]; board.[i].[1] ]
    | i, j when i = height - 1 -> 
        [board.[i].[j - 1]; board.[i].[j + 1]; board.[i - 1].[j + 1]; board.[i - 1].[j - 1]; board.[i - 1].[j] ] 
    | i, j when j = width - 1 -> 
        [board.[i - 1].[j]; board.[i + 1].[j]; board.[i + 1].[j - 1]; board.[i - 1].[j - 1]; board.[i].[j - 1] ] 
    | i, j -> 
        [board.[i - 1].[j - 1]; board.[i - 1].[j]; board.[i - 1].[j + 1]; 
         board.[i + 1].[j - 1]; board.[i + 1].[j]; board.[i + 1].[j + 1];
         board.[i].[j - 1]; board.[i].[j + 1] ]
    
let countNeighborhoods : Expr[][] -> int -> int -> BitVecExpr -> BoolExpr = fun board i j c ->
    let rec count : Expr list -> BitVecExpr -> BoolExpr = fun exprs c ->
        match exprs with
        | [] -> Eq c (Int 0 4u)
        | expr :: exprs ->
            let c' = FreshVar 4u 
            let ite = 
                Ite (Eq expr (Int 1 1u)) 
                    (Eq c (Add c' (Int 1 4u)))
                    (Eq c c') :?> _
            And [|ite; count exprs c'|]
        
    count (getNeighborhoods board i j) c
    
    
let rules : Expr[][] -> Expr[][] -> BoolExpr = fun fromBoard toBoard ->
    And [| for i in {0..height - 1} ->
            And [| for j in {0..width - 1} do
                        let c = IntVar (sprintf "C_%d_%d" i j) 4u 
                        let b = countNeighborhoods fromBoard i j c
                        let ite = 
                            Ite (Eq fromBoard.[i].[j] (Int 1 1u))
                                (Ite (Or [|(Eq c (Int 0 4u)); (Eq c (Int 1 4u)) |])
                                     (Eq toBoard.[i].[j] (Int 0 1u))
                                     (Ite (Or [|(Eq c (Int 4 4u)); (Eq c (Int 5 4u)); 
                                                (Eq c (Int 6 4u)); (Eq c (Int 7 4u)); (Eq c (Int 8 4u)) |])
                                        (Eq toBoard.[i].[j] (Int 0 1u))
                                        (Ite (Or [|(Eq c (Int 2 4u)); (Eq c (Int 3 4u))|])
                                             (Eq toBoard.[i].[j] (Int 1 1u))
                                            False))) 
                                (Ite (Eq c (Int 3 4u)) 
                                    (Eq toBoard.[i].[j] (Int 1 1u))
                                    (Eq toBoard.[i].[j] (Int 0 1u))) :?> _
                        yield And [|b; ite|] |] |]
    

let formula = And [|validValues; validFinalPattern; rules initBoard finalBoard|]

let solver = ctx.MkSolver()
solver.Assert(formula)
let flag = solver.Check() = Status.SATISFIABLE

let model = solver.Model


for i in {0..height - 1} do
    for j in {0..width - 1} do
        let value = string <| model.Evaluate(initBoard.[i].[j])
        let c = getNeighborhoods initBoard i j 
                |> List.filter (fun x -> string <| model.Evaluate(x) = "1") 
                |> List.length
        match value with
        | "1" -> 
            match c with
            | 0 | 1 -> printf " "
            | 4 | 5 | 6 | 7 | 8 -> printf " "
            | 2 | 3 -> printf "1"
            | _ -> failwith "oups"
        | "0" -> 
            match c with
            | 3 -> printf "1"
            | _ -> printf " "
        | _ -> failwith "oups"

    printfn ""









