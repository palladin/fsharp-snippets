// Eternity 2 Solver via Z3
// https://en.wikipedia.org/wiki/Eternity_II_puzzle

#time

#r "bin\Microsoft.Z3.dll"

open System
open System.IO
open System.Runtime.InteropServices
open System.Collections.Generic
open Microsoft.Z3


let setTimeout : float -> unit = fun secs -> 
    let timeout = TimeSpan.FromSeconds(secs).TotalMilliseconds
    Microsoft.Z3.Global.SetParameter("timeout", string timeout)

Microsoft.Z3.Global.ToggleWarningMessages(true)
//Microsoft.Z3.Global.SetParameter("parallel.enable", "false")
Microsoft.Z3.Global.SetParameter("model_validate", "true")
Microsoft.Z3.Global.SetParameter("proof", "true")
setTimeout(120.0)
Microsoft.Z3.Global.SetParameter("model", "true")
printfn "%s" <| Microsoft.Z3.Version.ToString()

let ctx = new Context()

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


let strPiece : Piece -> string = fun piece -> new String([|piece.Down; piece.Left; piece.Up; piece.Right|])


//let dim = 4
//let puzzle = [|"YXXB"; "YBXX"; "XBBX"; "XYYX"; "UUUP"; "PUPP"; "UUPP"; "UUPP"; 
//               "YXYP"; "BXBP"; "YXBP"; "BXYP"; "YXYU"; "BXBU"; "BXYU"; "YXBU"|]
let dim = 16
let puzzle = [| "AQXX"; "AEXX"; "IQXX"; "QIXX"; "BAXA"; "JIXA"; "FAXA"; "FMXA"; "KQXA"; "GEXA";
                "OIXA"; "HEXA"; "HMXA"; "UEXA"; "JAXI"; "RQXI"; "NMXI"; "SMXI"; "GIXI"; "OIXI";
                "DEXI"; "LAXI"; "LMXI"; "TAXI"; "UAXI"; "BIXQ"; "BQXQ"; "JQXQ"; "RQXQ"; "GMXQ";
                "OIXQ"; "TQXQ"; "HIXQ"; "HEXQ"; "PMXQ"; "VEXQ"; "RAXE"; "CMXE"; "KMXE"; "SIXE";
                "SQXE"; "OAXE"; "OIXE"; "OQXE"; "DAXE"; "TEXE"; "HEXE"; "PEXE"; "BMXM"; "JAXM";
                "JIXM"; "FAXM"; "GEXM"; "DEXM"; "DMXM"; "HQXM"; "PAXM"; "PMXM"; "UIXM"; "VQXM";
                "FRBB"; "NGBB"; "JCBJ"; "BHBR"; "RVBR"; "NNBR"; "KJBR"; "TFBR"; "VHBR"; "CGBC";
                "GLBC"; "NRBK"; "ODBK"; "TOBK"; "HCBK"; "NOBS"; "SOBS"; "CPBG"; "TCBG"; "PUBG";
                "SRBO"; "RRBD"; "KDBD"; "RSBL"; "FNBL"; "HLBL"; "PTBL"; "BUBT"; "FVBT"; "DPBT";
                "KLBH"; "SOBH"; "SDBH"; "DUBH"; "LNBH"; "UCBU"; "DSBV"; "THBV"; "UFBV"; "VUBV";
                "LOJJ"; "LPJJ"; "PSJJ"; "VFJJ"; "DOJR"; "CHJF"; "SHJF"; "DOJF"; "PKJF"; "OLJN";
                "LOJN"; "TSJC"; "TPJC"; "NDJK"; "GLJK"; "LKJK"; "VPJK"; "CUJS"; "PLJG"; "HVJO";
                "NVJD"; "FPJT"; "NSJT"; "TOJT"; "LVJH"; "UOJH"; "NFJP"; "SUJP"; "DCJP"; "THJP";
                "FTJU"; "LNJU"; "NPJV"; "KDJV"; "DCJV"; "PTJV"; "TGRR"; "FCRF"; "FKRF"; "FLRF";
                "SURF"; "OFRF"; "PLRF"; "UURF"; "CDRN"; "RLRC"; "RVRC"; "CNRC"; "OLRC"; "FKRS";
                "DVRS"; "KKRG"; "KSRG"; "VPRG"; "GGRD"; "GLRD"; "VGRD"; "GPRT"; "HFRT"; "UURH";
                "FTRP"; "NTRP"; "OKRV"; "DPRV"; "CDFN"; "DHFN"; "CCFK"; "KOFS"; "SUFS"; "DHFG";
                "TPFG"; "UKFG"; "OOFO"; "LTFO"; "GUFD"; "GSFL"; "NDFT"; "LPFH"; "HOFH"; "GPFP";
                "KPFU"; "GKFU"; "SHNN"; "VGNC"; "SLNK"; "HHNK"; "UGNS"; "NUNG"; "CSNG"; "PSNG";
                "CCNO"; "OTNO"; "KGND"; "UKNL"; "UVNL"; "VONL"; "KVNT"; "SHNT"; "TTNT"; "SCNH";
                "UHNP"; "VGNP"; "LSNU"; "LHNU"; "PCNU"; "VUNU"; "VGCC"; "SVCK"; "HOCK"; "KSCG";
                "POCG"; "CPCO"; "HHCD"; "CTCL"; "DVCL"; "VUCL"; "SOCT"; "DLCP"; "KDCU"; "KPCV";
                "UUCV"; "UVCV"; "LVKK"; "TGKK"; "POKK"; "SOKG"; "LLKG"; "SHKD"; "GVKT"; "PHKT";
                "LTKH"; "LUKH"; "STSS"; "PDSG"; "GDSD"; "GTSD"; "LOSD"; "DPSL"; "OVST"; "UOST";
                "GUSH"; "DUSH"; "OLGO"; "THGO"; "VTGD"; "PVGU"; "UVOO"; "LDOD"; "DUOL"; "PUOT";
                "VHDD"; "HLDL"; "PTLH"; "UPTP"; "PVTV"; "UVHV" |]



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

let solver = ctx.MkSolver("QF_BV")
solver.Assert(formula)
let r = solver.Check()

if r = Status.SATISFIABLE then

    let model = solver.Model

    for i in {0..dim - 1} do
        for j in {0..dim - 1} do
            let value = string <| model.Evaluate(IntVar (sprintf "Y_%d_%d" i j) 8u)
            printf "%s " value
        printfn ""

    printfn ""

    for i in {0..dim - 1} do
        for j in {0..dim - 1} do
            let value = string <| model.Evaluate(IntVar (sprintf "X_%d_%d" i j) 8u)
            printf "%s " value
        printfn ""

    for i in {0..dim - 1} do
        for j in {0..dim - 1} do
            let value = Int32.Parse(string <| model.Evaluate(IntVar (sprintf "X_%d_%d" i j) 8u))
            let piece = pieces |> Array.collect (fun piece -> piece) |> Array.find (fun piece -> piece.RotationId = value)
            printf "%s " <| strPiece piece
        printfn ""

else if r = Status.UNSATISFIABLE then
    printf "Proof: %A" solver.Proof
    for core in solver.UnsatCore do
        printf "Unsat core: %A" core

else printfn "unknown"









