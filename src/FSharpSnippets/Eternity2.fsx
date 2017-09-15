// Eternity 2 Solver via Z3
// https://en.wikipedia.org/wiki/Eternity_II_puzzle

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
let FreshVar : unit -> uint32 -> BitVecExpr = fun () w -> ctx.MkBVConst(Guid.NewGuid().ToString(), w) 
let Eq : Expr -> Expr -> BoolExpr = fun l r -> ctx.MkEq(l, r) 
let Ite : BoolExpr -> Expr -> Expr -> Expr = fun p t e -> ctx.MkITE(p, t, e)
let And : BoolExpr[] -> BoolExpr = fun bools -> ctx.MkAnd(bools)
let Or : BoolExpr[] -> BoolExpr = fun bools -> ctx.MkOr(bools)
let Not : BoolExpr -> BoolExpr = fun bool -> ctx.MkNot(bool)
let Distinct : Expr[] -> BoolExpr = fun exprs -> ctx.MkDistinct(exprs)


type Piece = { Id : int; RotationId : int; Up : char; Down : char; Left : char; Right : char }

let rotations : string -> int -> Piece[] = fun piece index ->
    let piece = { Id = index; RotationId = 0; Up = piece.[2]; Down = piece.[0]; Left = piece.[1]; Right = piece.[3]  }
    [| piece;
       { Id = index; RotationId = 0; Up = piece.Left; Down = piece.Right; Left = piece.Down; Right = piece.Up  }
       { Id = index; RotationId = 0; Up = piece.Down; Down = piece.Up; Left = piece.Right; Right = piece.Left  }
       { Id = index; RotationId = 0; Up = piece.Right; Down = piece.Left; Left = piece.Up; Right = piece.Down  }|]

let dim = 4
let puzzle = [|"YXXB"; "YBXX"; "XBBX"; "XYYX"; "UUUP"; "PUPP"; "UUPP"; "UUPP"; 
               "YXYP"; "BXBP"; "YXBP"; "BXYP"; "YXYU"; "BXBU"; "BXYU"; "YXBU"|]
let pieces = 
    puzzle
    |> Array.mapi (fun i piece -> rotations piece i)
    |> Array.collect id
    |> Array.mapi (fun i piece -> { piece with RotationId = i })
    |> Array.splitInto dim 


let varPieces : Expr[][] =
    [| for i in {0..dim - 1} ->
        [| for j in {0..dim - 1} -> IntVar (sprintf "X_%d_%d" i j) 8u :> _ |] |]

let validValues : BoolExpr =
    let pieces = pieces |> Array.collect id
    And [| for i in {0..dim - 1} do
            for j in {0..dim - 1} do 
                yield Or [| for piece in pieces do yield Eq (varPieces.[i].[j]) (Int piece.RotationId 8u) |] |]

let distinct = 
    let pieces = pieces |> Array.collect id
    let tempVars = 
        [| for i in {0..dim - 1} ->
            [| for j in {0..dim - 1} -> IntVar (sprintf "Y_%d_%d" i j) 8u |] |]
    let maps =
        And [| for i in {0..dim - 1} do
                for j in {0..dim - 1} do 
                    for piece in pieces do 
                        yield Ite (Eq varPieces.[i].[j] (Int piece.RotationId 8u)) (Eq tempVars.[i].[j] (Int piece.Id 8u)) True :?> _ |]
    
    let distinctValues = tempVars |> Array.collect id |> Array.map (fun v -> v :> Expr) |> Distinct
    And [|maps; distinctValues|]

let constraints = 
    let n = dim - 1
    let pieces = pieces |> Array.collect id
    let findUp c = pieces |> Array.filter (fun piece -> piece.Up = c) 
    let findDown c = pieces |> Array.filter (fun piece -> piece.Down = c) 
    let findLeft c = pieces |> Array.filter (fun piece -> piece.Left = c) 
    let findRight c = pieces |> Array.filter (fun piece -> piece.Right = c) 
    And [| for i in {0..n} do
                for j in {0..n} do 
                    match i, j with
                    | 0, 0 -> 
                        let pieces = pieces |> Array.filter (fun piece -> piece.Up = 'X' && piece.Left = 'X')
                        yield Or [| for piece in pieces do
                                        yield And [| Eq varPieces.[0].[0] (Int piece.RotationId 8u);
                                                     Or (piece.Down |> findUp |> Array.filter (fun piece -> piece.Left = 'X') |> Array.map (fun piece -> Eq varPieces.[1].[0] (Int piece.RotationId 8u)));
                                                     Or (piece.Right |> findLeft |> Array.filter (fun piece -> piece.Up = 'X') |> Array.map (fun piece -> Eq varPieces.[0].[1] (Int piece.RotationId 8u))) |]
                                 |]
                    | 0, j when j = n -> 
                        let pieces = pieces |> Array.filter (fun piece -> piece.Up = 'X' && piece.Right = 'X')
                        yield Or [| for piece in pieces do
                                        yield And [| Eq varPieces.[0].[n] (Int piece.RotationId 8u);
                                                     Or (piece.Left |> findRight |> Array.filter (fun piece -> piece.Up = 'X') |> Array.map (fun piece -> Eq varPieces.[0].[n - 1] (Int piece.RotationId 8u)));
                                                     Or (piece.Down |> findUp |> Array.filter (fun piece -> piece.Right = 'X') |> Array.map (fun piece -> Eq varPieces.[1].[n] (Int piece.RotationId 8u))) |]
                                 |]
                    | i, 0 when i = n ->
                        let pieces = pieces |> Array.filter (fun piece -> piece.Down = 'X' && piece.Left = 'X')
                        yield Or [| for piece in pieces do
                                        yield And [| Eq varPieces.[n].[0] (Int piece.RotationId 8u);
                                                     Or (piece.Up |> findDown |> Array.filter (fun piece -> piece.Left = 'X') |> Array.map (fun piece -> Eq varPieces.[n - 1].[0] (Int piece.RotationId 8u)));
                                                     Or (piece.Right |> findLeft |> Array.filter (fun piece -> piece.Down = 'X') |> Array.map (fun piece -> Eq varPieces.[n].[1] (Int piece.RotationId 8u))) |]
                                 |]
                    | i, j when i = n && j = n -> 
                        let pieces = pieces |> Array.filter (fun piece -> piece.Down = 'X' && piece.Right = 'X')
                        yield Or [| for piece in pieces do
                                        yield And [| Eq varPieces.[n].[n] (Int piece.RotationId 8u);
                                                     Or (piece.Up |> findDown |> Array.filter (fun piece -> piece.Right = 'X') |> Array.map (fun piece -> Eq varPieces.[n - 1].[n] (Int piece.RotationId 8u)));
                                                     Or (piece.Left |> findRight |> Array.filter (fun piece -> piece.Down = 'X') |> Array.map (fun piece -> Eq varPieces.[n].[n - 1] (Int piece.RotationId 8u))) |]
                                 |]
                    | 0, j ->
                        let pieces = pieces |> Array.filter (fun piece -> piece.Up = 'X' )
                        yield Or [| for piece in pieces do
                                        yield And [| Eq varPieces.[0].[j] (Int piece.RotationId 8u);
                                                     Or (piece.Down |> findUp |> Array.map (fun piece -> Eq varPieces.[1].[j] (Int piece.RotationId 8u)))
                                                     Or (piece.Left |> findRight |> Array.filter (fun piece -> piece.Up = 'X') |> Array.map (fun piece -> Eq varPieces.[0].[j - 1] (Int piece.RotationId 8u)));
                                                     Or (piece.Right |> findLeft |> Array.filter (fun piece -> piece.Up = 'X') |> Array.map (fun piece -> Eq varPieces.[0].[j + 1] (Int piece.RotationId 8u))) |]
                                 |]
                    | i, 0 ->
                        let pieces = pieces |> Array.filter (fun piece -> piece.Left = 'X' )
                        yield Or [| for piece in pieces do
                                        yield And [| Eq varPieces.[i].[0] (Int piece.RotationId 8u);
                                                     Or (piece.Right |> findLeft |> Array.map (fun piece -> Eq varPieces.[i].[1] (Int piece.RotationId 8u)))
                                                     Or (piece.Up |> findDown |> Array.filter (fun piece -> piece.Left = 'X') |> Array.map (fun piece -> Eq varPieces.[i - 1].[0] (Int piece.RotationId 8u)));
                                                     Or (piece.Down |> findUp |> Array.filter (fun piece -> piece.Left = 'X') |> Array.map (fun piece -> Eq varPieces.[i + 1].[0] (Int piece.RotationId 8u))) |]
                                 |]
                    | i, j when i = n ->
                        let pieces = pieces |> Array.filter (fun piece -> piece.Down = 'X' )
                        yield Or [| for piece in pieces do
                                        yield And [| Eq varPieces.[n].[j] (Int piece.RotationId 8u);
                                                     Or (piece.Up |> findDown |> Array.map (fun piece -> Eq varPieces.[n - 1].[j] (Int piece.RotationId 8u)))
                                                     Or (piece.Left |> findRight |> Array.filter (fun piece -> piece.Down = 'X') |> Array.map (fun piece -> Eq varPieces.[n].[j - 1] (Int piece.RotationId 8u)));
                                                     Or (piece.Right |> findLeft |> Array.filter (fun piece -> piece.Down = 'X') |> Array.map (fun piece -> Eq varPieces.[n].[j + 1] (Int piece.RotationId 8u))) |]
                                 |]
                    | i, j when j = n ->
                        let pieces = pieces |> Array.filter (fun piece -> piece.Right = 'X' )
                        yield Or [| for piece in pieces do
                                        yield And [| Eq varPieces.[i].[n] (Int piece.RotationId 8u);
                                                     Or (piece.Left |> findRight |> Array.map (fun piece -> Eq varPieces.[i].[n - 1] (Int piece.RotationId 8u)))
                                                     Or (piece.Up |> findDown |> Array.filter (fun piece -> piece.Right = 'X') |> Array.map (fun piece -> Eq varPieces.[i - 1].[n] (Int piece.RotationId 8u)));
                                                     Or (piece.Down |> findUp |> Array.filter (fun piece -> piece.Right = 'X') |> Array.map (fun piece -> Eq varPieces.[i + 1].[n] (Int piece.RotationId 8u))) |]
                                 |]
                    | i, j -> 
                        let pieces = pieces |> Array.filter (fun piece -> piece.Up <> 'X' && piece.Down <> 'X' && piece.Left <> 'X' && piece.Right <> 'X' )
                        yield Or [| for piece in pieces do
                                        yield And [| Eq varPieces.[i].[j] (Int piece.RotationId 8u);
                                                     Or (piece.Up |> findDown |> Array.map (fun piece -> Eq varPieces.[i - 1].[j] (Int piece.RotationId 8u)))
                                                     Or (piece.Down |> findUp |> Array.map (fun piece -> Eq varPieces.[i + 1].[j] (Int piece.RotationId 8u)));
                                                     Or (piece.Left |> findRight |> Array.map (fun piece -> Eq varPieces.[i].[j - 1] (Int piece.RotationId 8u)));
                                                     Or (piece.Right |> findLeft |> Array.map (fun piece -> Eq varPieces.[i].[j + 1] (Int piece.RotationId 8u))) |]
                                 |] |]

let formula = And [|validValues; distinct; constraints|]

let solver = ctx.MkSolver()
solver.Assert(formula)
let flag = solver.Check() = Status.SATISFIABLE

let model = solver.Model

for i in {0..dim - 1} do
    for j in {0..dim - 1} do
        let value = string <| model.Evaluate(IntVar (sprintf "Y_%d_%d" i j) 8u)
        printf "%s " value
    printfn ""









