open Stdio
let rec accumulate f acc sequence =
  match sequence with
  | [] -> acc
  | x::xs -> f x (accumulate f acc xs)

(* https://en.wikipedia.org/wiki/Cons *)
let cons x xs = 
  x::xs

let map p sequence = 
  accumulate p [] sequence

let append seq1 seq2 =
  accumulate cons seq2 seq1

let length sequence =
  accumulate (fun _ x -> x + 1) 0 sequence


let()=
printf "%N\n" (length [1;2;3]);;
printf "%N\n" (length [1;2;4;3])
