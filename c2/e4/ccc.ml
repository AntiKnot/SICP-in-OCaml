(* open Base *)
(* open Stdio *)

let cons x y m = 
  m x y
let car z = 
  (* z (fun p q -> p) *)
  z (fun p _ -> p)
let cdr z = 
  (* z (fun p q -> q) *)
  z (fun _ q -> q)

(* Returns a partial function, ready to accept a function parameter,
This function will be called with the first two parameters as parameters 
cons 1 2 -> fun m -> m 1 2
car (cons 1 2) -> (fun p _ -> p) 1 2 -> 1
cdr (cons 1 2) -> (fun p _ -> p) 1 2 -> 2
*)