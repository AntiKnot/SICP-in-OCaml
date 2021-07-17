open Base
open Stdio

(* The guess function, improvement function, and verification function are all fixed frames.
Square root and cube root simply replace the improvement function. *)

let good_enough old_guess new_guess = 
  equal (compare_float (Float.abs((new_guess -. old_guess) /. old_guess)) 0.001) (-1)
let improve guess x = 
  (x /. (guess **. 2.) +. 2.*.guess) /. 3.
let rec qube_iter guess x = 
  if (good_enough guess (improve guess x)) then (improve guess x) 
  else (qube_iter (improve guess x) x)
let myquberoot x = 
  qube_iter 1.0 x 

let () =
  printf "sqrt: %F\n" (myquberoot 27.0)



