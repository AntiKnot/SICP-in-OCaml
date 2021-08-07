open Stdio

let rec accumulate f acc sequence =
  match sequence with
  | [] -> acc
  | x::xs -> f x (accumulate f acc xs)
let horner n l = 
  accumulate (fun x y-> y*n+x) 0 l

let ()=
  printf "%N\n" (horner 2 [1;3;0;5;0;1]);



