open Base 
open Stdio

let rec sum term a next b =
  if a > b then 0
  else term a + sum term (next a) next b 

(* Further abstraction this segment, `sum` is `accumulate`,`0` is `null-value`,`+` is `combiner` *)

let rec accumulate combiner null_value term a next b =
  if a > b then null_value
  else  combiner (term a) (accumulate combiner null_value term (next a) next b)

let product term a next b = 
  accumulate (fun x y -> x*y) 1 term a next b 

(* It’s easy to understand when the new feature “folding” appears later. *)
(* Unlike lisp, ocaml is a statically typed language, and paradigm computing needs to be further abstracted *)