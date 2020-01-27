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
//setTimeout(120.0)
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
    [| for i = 0 to piece.Length - 1 do
        let piece =  piece.Substring(i) + piece.Substring(0, i)
        yield { Id = index; RotationId = 0; Up = piece.[0] ; Right = piece.[1]; Down = piece.[2]; Left = piece.[3];  }|]


let strPiece : Piece -> string = fun piece -> sprintf "%c%c%c%c" piece.Up piece.Right piece.Down piece.Left

//let dim = 4
//let puzzle = [|"YXXB"; "YBXX"; "XBBX"; "XYYX"; "UUUP"; "PUPP"; "UUPP"; "UUPP"; 
//               "YXYP"; "BXBP"; "YXBP"; "BXYP"; "YXYU"; "BXBU"; "BXYU"; "YXBU"|]
let dim = 16

// https://github.com/AntonFagerberg/Eternity-II-Solver/blob/master/src/main/scala/com/example/Game.scala
// directions: up, right, down, left
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

// https://github.com/vaga/Goternity/blob/master/assets/pieces.txt
// directions: north, south, west, east 
//let puzzle' = 
//    [|[|1; 0; 0; 17|]; [|1; 0; 0; 5|]; [|9; 0; 0; 17|]; [|17; 0; 0; 9|];
//        [|2; 0; 1; 1|]; [|10; 0; 1; 9|]; [|6; 0; 1; 1|]; [|6; 0; 1; 13|];
//        [|11; 0; 1; 17|]; [|7; 0; 1; 5|]; [|15; 0; 1; 9|]; [|8; 0; 1; 5|];
//        [|8; 0; 1; 13|]; [|21; 0; 1; 5|]; [|10; 0; 9; 1|]; [|18; 0; 9; 17|];
//        [|14; 0; 9; 13|]; [|19; 0; 9; 13|]; [|7; 0; 9; 9|]; [|15; 0; 9; 9|];
//        [|4; 0; 9; 5|]; [|12; 0; 9; 1|]; [|12; 0; 9; 13|]; [|20; 0; 9; 1|];
//        [|21; 0; 9; 1|]; [|2; 0; 17; 9|]; [|2; 0; 17; 17|]; [|10; 0; 17; 17|];
//        [|18; 0; 17; 17|]; [|7; 0; 17; 13|]; [|15; 0; 17; 9|]; [|20; 0; 17; 17|];
//        [|8; 0; 17; 9|]; [|8; 0; 17; 5|]; [|16; 0; 17; 13|]; [|22; 0; 17; 5|];
//        [|18; 0; 5; 1|]; [|3; 0; 5; 13|]; [|11; 0; 5; 13|]; [|19; 0; 5; 9|];
//        [|19; 0; 5; 17|]; [|15; 0; 5; 1|]; [|15; 0; 5; 9|]; [|15; 0; 5; 17|];
//        [|4; 0; 5; 1|]; [|20; 0; 5; 5|]; [|8; 0; 5; 5|]; [|16; 0; 5; 5|];
//        [|2; 0; 13; 13|]; [|10; 0; 13; 1|]; [|10; 0; 13; 9|]; [|6; 0; 13; 1|];
//        [|7; 0; 13; 5|]; [|4; 0; 13; 5|]; [|4; 0; 13; 13|]; [|8; 0; 13; 17|];
//        [|16; 0; 13; 1|]; [|16; 0; 13; 13|]; [|21; 0; 13; 9|]; [|22; 0; 13; 17|];
//        [|6; 2; 2; 18|]; [|14; 2; 2; 7|]; [|10; 2; 10; 3|]; [|2; 2; 18; 8|];
//        [|18; 2; 18; 22|]; [|14; 2; 18; 14|]; [|11; 2; 18; 10|]; [|20; 2; 18; 6|];
//        [|22; 2; 18; 8|]; [|3; 2; 3; 7|]; [|7; 2; 3; 12|]; [|14; 2; 11; 18|];
//        [|15; 2; 11; 4|]; [|20; 2; 11; 15|]; [|8; 2; 11; 3|]; [|14; 2; 19; 15|];
//        [|19; 2; 19; 15|]; [|3; 2; 7; 16|]; [|20; 2; 7; 3|]; [|16; 2; 7; 21|];
//        [|19; 2; 15; 18|]; [|18; 2; 4; 18|]; [|11; 2; 4; 4|]; [|18; 2; 12; 19|];
//        [|6; 2; 12; 14|]; [|8; 2; 12; 12|]; [|16; 2; 12; 20|]; [|2; 2; 20; 21|];
//        [|6; 2; 20; 22|]; [|4; 2; 20; 16|]; [|11; 2; 8; 12|]; [|19; 2; 8; 15|];
//        [|19; 2; 8; 4|]; [|4; 2; 8; 21|]; [|12; 2; 8; 14|]; [|21; 2; 21; 3|];
//        [|4; 2; 22; 19|]; [|20; 2; 22; 8|]; [|21; 2; 22; 6|]; [|22; 2; 22; 21|];
//        [|12; 10; 10; 15|]; [|12; 10; 10; 16|]; [|16; 10; 10; 19|];
//        [|22; 10; 10; 6|]; [|4; 10; 18; 15|]; [|3; 10; 6; 8|]; [|19; 10; 6; 8|];
//        [|4; 10; 6; 15|]; [|16; 10; 6; 11|]; [|15; 10; 14; 12|];
//        [|12; 10; 14; 15|]; [|20; 10; 3; 16|]; [|20; 10; 3; 19|];
//        [|14; 10; 11; 4|]; [|7; 10; 11; 12|]; [|12; 10; 11; 11|];
//        [|22; 10; 11; 16|]; [|3; 10; 19; 21|]; [|16; 10; 7; 12|];
//        [|8; 10; 15; 22|]; [|14; 10; 4; 22|]; [|6; 10; 20; 16|];
//        [|14; 10; 20; 19|]; [|20; 10; 20; 15|]; [|12; 10; 8; 22|];
//        [|21; 10; 8; 15|]; [|14; 10; 16; 6|]; [|19; 10; 16; 21|]; [|4; 10; 16; 3|];
//        [|20; 10; 16; 8|]; [|6; 10; 21; 20|]; [|12; 10; 21; 14|];
//        [|14; 10; 22; 16|]; [|11; 10; 22; 4|]; [|4; 10; 22; 3|];
//        [|16; 10; 22; 20|]; [|20; 18; 18; 7|]; [|6; 18; 6; 3|]; [|6; 18; 6; 11|];
//        [|6; 18; 6; 12|]; [|19; 18; 6; 21|]; [|15; 18; 6; 6|]; [|16; 18; 6; 12|];
//        [|21; 18; 6; 21|]; [|3; 18; 14; 4|]; [|18; 18; 3; 12|]; [|18; 18; 3; 22|];
//        [|3; 18; 3; 14|]; [|15; 18; 3; 12|]; [|6; 18; 19; 11|]; [|4; 18; 19; 22|];
//        [|11; 18; 7; 11|]; [|11; 18; 7; 19|]; [|22; 18; 7; 16|]; [|7; 18; 4; 7|];
//        [|7; 18; 4; 12|]; [|22; 18; 4; 7|]; [|7; 18; 20; 16|]; [|8; 18; 20; 6|];
//        [|21; 18; 8; 21|]; [|6; 18; 16; 20|]; [|14; 18; 16; 20|];
//        [|15; 18; 22; 11|]; [|4; 18; 22; 16|]; [|3; 6; 14; 4|]; [|4; 6; 14; 8|];
//        [|3; 6; 11; 3|]; [|11; 6; 19; 15|]; [|19; 6; 19; 21|]; [|4; 6; 7; 8|];
//        [|20; 6; 7; 16|]; [|21; 6; 7; 11|]; [|15; 6; 15; 15|]; [|12; 6; 15; 20|];
//        [|7; 6; 4; 21|]; [|7; 6; 12; 19|]; [|14; 6; 20; 4|]; [|12; 6; 8; 16|];
//        [|8; 6; 8; 15|]; [|7; 6; 16; 16|]; [|11; 6; 21; 16|]; [|21; 11; 6; 7|];
//        [|14; 8; 14; 19|]; [|22; 14; 3; 7|]; [|19; 14; 11; 12|]; [|8; 14; 11; 8|];
//        [|21; 14; 19; 7|]; [|14; 14; 7; 21|]; [|3; 14; 7; 19|]; [|16; 14; 7; 19|];
//        [|3; 14; 15; 3|]; [|15; 14; 15; 20|]; [|11; 14; 4; 7|]; [|21; 14; 12; 11|];
//        [|21; 14; 12; 22|]; [|22; 14; 12; 15|]; [|11; 14; 20; 22|];
//        [|19; 14; 20; 8|]; [|20; 14; 20; 20|]; [|19; 14; 8; 3|];
//        [|21; 14; 16; 8|]; [|22; 14; 16; 7|]; [|12; 14; 21; 19|];
//        [|12; 14; 21; 8|]; [|16; 14; 21; 3|]; [|22; 14; 21; 21|]; [|22; 3; 3; 7|];
//        [|19; 3; 11; 22|]; [|8; 3; 11; 15|]; [|11; 3; 7; 19|]; [|16; 3; 7; 15|];
//        [|3; 3; 15; 16|]; [|8; 3; 4; 8|]; [|3; 3; 12; 20|]; [|4; 3; 12; 22|];
//        [|22; 3; 12; 21|]; [|19; 3; 20; 15|]; [|4; 3; 16; 12|]; [|11; 3; 21; 4|];
//        [|11; 3; 22; 16|]; [|21; 3; 22; 21|]; [|21; 3; 22; 22|];
//        [|12; 11; 11; 22|]; [|20; 11; 11; 7|]; [|16; 11; 11; 15|];
//        [|19; 11; 7; 15|]; [|12; 11; 7; 12|]; [|19; 11; 4; 8|]; [|7; 11; 20; 22|];
//        [|16; 11; 20; 8|]; [|12; 11; 8; 20|]; [|12; 11; 8; 21|];
//        [|19; 19; 19; 20|]; [|16; 19; 7; 4|]; [|7; 19; 4; 4|]; [|7; 19; 4; 20|];
//        [|12; 19; 4; 15|]; [|4; 19; 12; 16|]; [|15; 19; 20; 22|];
//        [|21; 19; 20; 15|]; [|7; 19; 8; 21|]; [|4; 19; 8; 21|]; [|15; 7; 15; 12|];
//        [|20; 7; 15; 8|]; [|22; 7; 4; 20|]; [|16; 7; 21; 22|]; [|21; 15; 15; 22|];
//        [|12; 15; 4; 4|]; [|4; 15; 12; 21|]; [|16; 15; 20; 21|]; [|22; 4; 4; 8|];
//        [|8; 4; 12; 12|]; [|16; 12; 8; 20|]; [|21; 20; 16; 16|];
//        [|16; 20; 22; 22|]; [|21; 8; 22; 22|]
//    |] |> Array.map (fun xs -> let map = Array.append [|'X'|] [|'A'..'W'|] in sprintf "%c%c%c%c" map.[xs.[0]] map.[xs.[3]] map.[xs.[1]] map.[xs.[2]])


//(puzzle, puzzle') ||> Array.zip |> Array.filter (fun (x, y) -> x <> y)

// https://github.com/lumy/EternityII/blob/master/test_16pieces.txt
// directions: north, south, west, east
//let puzzle = 
//    [| [|0; 2; 0; 1|]; [|0; 6; 1; 8|]; [|0; 4; 8; 3|]; [|0; 4; 3; 0|];
//       [|2; 12; 0; 17|]; [|6; 8; 17; 4|]; [|4; 8; 4; 10|]; [|4; 5; 10; 0|];
//       [|12; 3; 0; 11|]; [|8; 11; 11; 7|]; [|8; 4; 7; 8|]; [|5; 1; 8; 0|];
//       [|3; 0; 6; 0|]; [|11; 0; 6; 3|]; [|4; 0; 3; 7|]; [|1; 0; 7; 0|] 
//    |] |> Array.map (fun xs -> let map = Array.append [|'X'|] [|'A'..'W'|] in sprintf "%c%c%c%c" map.[xs.[0]] map.[xs.[3]] map.[xs.[1]] map.[xs.[2]])

let pieces = 
    puzzle
    |> Array.mapi (fun i piece -> rotations piece i)
    |> Array.collect id
    |> Array.mapi (fun i piece -> { piece with RotationId = i })
    |> Array.splitInto dim 


let varPieces : Expr[][] =
    [| for i in {0..dim - 1} ->
        [| for j in {0..dim - 1} -> IntVar (sprintf "RotPiece_%d_%d" i j) 16u :> _ |] |]

let validValues : BoolExpr =
    let pieces = pieces |> Array.collect id
    And [| for i in {0..dim - 1} do
            for j in {0..dim - 1} do 
                yield Or [| for piece in pieces do yield Eq (varPieces.[i].[j]) (Int piece.RotationId 16u) |] |]

let distinct = 
    let pieces = pieces |> Array.collect id
    let tempVars = 
        [| for i in {0..dim - 1} ->
            [| for j in {0..dim - 1} -> IntVar (sprintf "Piece_%d_%d" i j) 16u |] |]
    let maps =
        And [| for i in {0..dim - 1} do
                for j in {0..dim - 1} do 
                    for piece in pieces do 
                        yield Ite (Eq varPieces.[i].[j] (Int piece.RotationId 16u)) (Eq tempVars.[i].[j] (Int piece.Id 16u)) True :?> _ |]
    
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
                                        yield And [| Eq varPieces.[0].[0] (Int piece.RotationId 16u);
                                                     Or (piece.Down |> findUp |> Array.filter (fun piece -> piece.Left = 'X') |> Array.map (fun piece -> Eq varPieces.[1].[0] (Int piece.RotationId 16u)));
                                                     Or (piece.Right |> findLeft |> Array.filter (fun piece -> piece.Up = 'X') |> Array.map (fun piece -> Eq varPieces.[0].[1] (Int piece.RotationId 16u))) |]
                                 |]
                    | 0, j when j = n -> 
                        let pieces = pieces |> Array.filter (fun piece -> piece.Up = 'X' && piece.Right = 'X')
                        yield Or [| for piece in pieces do
                                        yield And [| Eq varPieces.[0].[n] (Int piece.RotationId 16u);
                                                     Or (piece.Left |> findRight |> Array.filter (fun piece -> piece.Up = 'X') |> Array.map (fun piece -> Eq varPieces.[0].[n - 1] (Int piece.RotationId 16u)));
                                                     Or (piece.Down |> findUp |> Array.filter (fun piece -> piece.Right = 'X') |> Array.map (fun piece -> Eq varPieces.[1].[n] (Int piece.RotationId 16u))) |]
                                 |]
                    | i, 0 when i = n ->
                        let pieces = pieces |> Array.filter (fun piece -> piece.Down = 'X' && piece.Left = 'X')
                        yield Or [| for piece in pieces do
                                        yield And [| Eq varPieces.[n].[0] (Int piece.RotationId 16u);
                                                     Or (piece.Up |> findDown |> Array.filter (fun piece -> piece.Left = 'X') |> Array.map (fun piece -> Eq varPieces.[n - 1].[0] (Int piece.RotationId 16u)));
                                                     Or (piece.Right |> findLeft |> Array.filter (fun piece -> piece.Down = 'X') |> Array.map (fun piece -> Eq varPieces.[n].[1] (Int piece.RotationId 16u))) |]
                                 |]
                    | i, j when i = n && j = n -> 
                        let pieces = pieces |> Array.filter (fun piece -> piece.Down = 'X' && piece.Right = 'X')
                        yield Or [| for piece in pieces do
                                        yield And [| Eq varPieces.[n].[n] (Int piece.RotationId 16u);
                                                     Or (piece.Up |> findDown |> Array.filter (fun piece -> piece.Right = 'X') |> Array.map (fun piece -> Eq varPieces.[n - 1].[n] (Int piece.RotationId 16u)));
                                                     Or (piece.Left |> findRight |> Array.filter (fun piece -> piece.Down = 'X') |> Array.map (fun piece -> Eq varPieces.[n].[n - 1] (Int piece.RotationId 16u))) |]
                                 |]
                    | 0, j ->
                        let pieces = pieces |> Array.filter (fun piece -> piece.Up = 'X' )
                        yield Or [| for piece in pieces do
                                        yield And [| Eq varPieces.[0].[j] (Int piece.RotationId 16u);
                                                     Or (piece.Down |> findUp |> Array.map (fun piece -> Eq varPieces.[1].[j] (Int piece.RotationId 16u)))
                                                     Or (piece.Left |> findRight |> Array.filter (fun piece -> piece.Up = 'X') |> Array.map (fun piece -> Eq varPieces.[0].[j - 1] (Int piece.RotationId 16u)));
                                                     Or (piece.Right |> findLeft |> Array.filter (fun piece -> piece.Up = 'X') |> Array.map (fun piece -> Eq varPieces.[0].[j + 1] (Int piece.RotationId 16u))) |]
                                 |]
                    | i, 0 ->
                        let pieces = pieces |> Array.filter (fun piece -> piece.Left = 'X' )
                        yield Or [| for piece in pieces do
                                        yield And [| Eq varPieces.[i].[0] (Int piece.RotationId 16u);
                                                     Or (piece.Right |> findLeft |> Array.map (fun piece -> Eq varPieces.[i].[1] (Int piece.RotationId 16u)))
                                                     Or (piece.Up |> findDown |> Array.filter (fun piece -> piece.Left = 'X') |> Array.map (fun piece -> Eq varPieces.[i - 1].[0] (Int piece.RotationId 16u)));
                                                     Or (piece.Down |> findUp |> Array.filter (fun piece -> piece.Left = 'X') |> Array.map (fun piece -> Eq varPieces.[i + 1].[0] (Int piece.RotationId 16u))) |]
                                 |]
                    | i, j when i = n ->
                        let pieces = pieces |> Array.filter (fun piece -> piece.Down = 'X' )
                        yield Or [| for piece in pieces do
                                        yield And [| Eq varPieces.[n].[j] (Int piece.RotationId 16u);
                                                     Or (piece.Up |> findDown |> Array.map (fun piece -> Eq varPieces.[n - 1].[j] (Int piece.RotationId 16u)))
                                                     Or (piece.Left |> findRight |> Array.filter (fun piece -> piece.Down = 'X') |> Array.map (fun piece -> Eq varPieces.[n].[j - 1] (Int piece.RotationId 16u)));
                                                     Or (piece.Right |> findLeft |> Array.filter (fun piece -> piece.Down = 'X') |> Array.map (fun piece -> Eq varPieces.[n].[j + 1] (Int piece.RotationId 16u))) |]
                                 |]
                    | i, j when j = n ->
                        let pieces = pieces |> Array.filter (fun piece -> piece.Right = 'X' )
                        yield Or [| for piece in pieces do
                                        yield And [| Eq varPieces.[i].[n] (Int piece.RotationId 16u);
                                                     Or (piece.Left |> findRight |> Array.map (fun piece -> Eq varPieces.[i].[n - 1] (Int piece.RotationId 16u)))
                                                     Or (piece.Up |> findDown |> Array.filter (fun piece -> piece.Right = 'X') |> Array.map (fun piece -> Eq varPieces.[i - 1].[n] (Int piece.RotationId 16u)));
                                                     Or (piece.Down |> findUp |> Array.filter (fun piece -> piece.Right = 'X') |> Array.map (fun piece -> Eq varPieces.[i + 1].[n] (Int piece.RotationId 16u))) |]
                                 |]
                    | i, j -> 
                        let pieces = pieces |> Array.filter (fun piece -> piece.Up <> 'X' && piece.Down <> 'X' && piece.Left <> 'X' && piece.Right <> 'X' )
                        yield Or [| for piece in pieces do
                                        yield And [| Eq varPieces.[i].[j] (Int piece.RotationId 16u);
                                                     Or (piece.Up |> findDown |> Array.map (fun piece -> Eq varPieces.[i - 1].[j] (Int piece.RotationId 16u)))
                                                     Or (piece.Down |> findUp |> Array.map (fun piece -> Eq varPieces.[i + 1].[j] (Int piece.RotationId 16u)));
                                                     Or (piece.Left |> findRight |> Array.map (fun piece -> Eq varPieces.[i].[j - 1] (Int piece.RotationId 16u)));
                                                     Or (piece.Right |> findLeft |> Array.map (fun piece -> Eq varPieces.[i].[j + 1] (Int piece.RotationId 16u))) |]
                                 |]
    |]


let formula = And [|validValues; distinct; constraints|]

let solver = ctx.MkSolver("QF_BV")
solver.Assert(formula)
let r = solver.Check()

if r = Status.SATISFIABLE then

    let model = solver.Model

    for i in {0..dim - 1} do
        for j in {0..dim - 1} do
            let value = string <| model.Evaluate(IntVar (sprintf "Piece_%d_%d" i j) 16u)
            printf "%s " value
        printfn ""

    printfn ""

    for i in {0..dim - 1} do
        for j in {0..dim - 1} do
            let value = string <| model.Evaluate(IntVar (sprintf "RotPiece_%d_%d" i j) 16u)
            printf "%s " value
        printfn ""

    for i in {0..dim - 1} do
        for j in {0..dim - 1} do
            let value = Int32.Parse(string <| model.Evaluate(IntVar (sprintf "RotPiece_%d_%d" i j) 16u))
            let piece = pieces |> Array.collect (fun piece -> piece) |> Array.find (fun piece -> piece.RotationId = value)
            printf "%s " <| strPiece piece
        printfn ""

else if r = Status.UNSATISFIABLE then
    printf "Proof: %A" solver.Proof
    for core in solver.UnsatCore do
        printf "Unsat core: %A" core

else printfn "unknown"
