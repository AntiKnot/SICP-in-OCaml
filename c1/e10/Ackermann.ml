open Base
open Stdio

(* 
In OCaml, what is the canonical way of matching against multiple arguments of a function?
https://stackoverflow.com/questions/8286001/in-ocaml-what-is-the-canonical-way-of-matching-against-multiple-arguments-of-a 

ackermann function
https://en.wikipedia.org/wiki/Ackermann_function
*)
let rec ack x y = 
  match (x,y) with 
  | (_,0) -> 0
  | (0,_) -> 2 * y
  | (_,1) -> 2
  | (_,_) -> ack (x-1) (ack  x (y-1))

let () = 
  printf "Total: %N\n" (ack 1 10);;
  printf "Total: %N\n" (ack 2 4);;
  printf "Total: %N\n" (ack 3 3);;

(* double y *)
let barf n = 
  ack 0 n

 (* x-1 y-1 when x=0 double y *)
let barg n = 
  ack 1 n

 (* x-1 y-1 when x=0 double y *)
let barh n = 
  ack 2 n

let bark n = 
  5*n*n

