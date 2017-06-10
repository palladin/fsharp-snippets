// From Löb's Theorem to Spreadsheet Evaluation (memoized)
// based on http://blog.sigfpe.com/2006/11/from-l-theorem-to-spreadsheet.html

type seq2<'T> = seq<seq<'T>>

let loeb : seq2<seq2<'T> -> 'T> -> seq2<'T> = fun seqs ->
    let rec g =  
        seq { for fs in seqs ->
                seq { for f in fs -> f g } |> Seq.cache } |> Seq.cache
    g

let value : 'T -> seq2<'T> -> 'T = 
    fun v _ -> v

let nth : (int * int) -> seq2<'T> -> 'T = 
    fun (i, j) cells -> cells |> Seq.item i |> Seq.item j


let cells : seq2<seq2<int> -> int> =
    [[value 1; nth (0, 0)];
     [value 2; fun c -> nth (1, 0) c + nth (0, 1) c];
     [value 3; fun c -> nth (2, 0) c + nth (1, 1) c];
     [value 4; fun c -> nth (3, 0) c + nth (2, 1) c];
     [value 5; fun c -> nth (4, 0) c + nth (3, 1) c];] 
    |> Seq.map Seq.cast 

cells |> loeb |> Seq.toArray
// [|seq [1; 1]; 
//   seq [2; 3]; 
//   seq [3; 6]; 
//   seq [4; 10]; 
//   seq [5; 15] |]