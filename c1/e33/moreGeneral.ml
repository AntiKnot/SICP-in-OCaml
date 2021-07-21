(* Predicate https://en.wikipedia.org/wiki/Predicate *)
open Base
open Stdio

let rec filter_accumulate predicate combiner null_value term a next b =
  if a > b then null_value
  else 
    combiner
    (if predicate a then (term a) else null_value) 
    (filter_accumulate predicate combiner null_value term (next a) next b)

(* Streaming computing will be introduced later. *)

let is_even n =  n % 2 = 0
let inc n = n+1
let square x = x*x

let even_acc a b =  
  filter_accumulate is_even (fun x y ->x+y) 0 square a inc b

let () = 
printf "sum square range[0,8] is: %N\n"  (even_acc 0 8)