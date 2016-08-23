// A simple Quine in F# 

let s : Printf.TextWriterFormat<_> = "let s : Printf.TextWriterFormat<_> = %c%s%c in
printf s (char 34) (s.Value) (char 34)" in printf s (char 34) (s.Value) (char 34)