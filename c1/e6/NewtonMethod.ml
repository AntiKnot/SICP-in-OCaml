open Base
open Stdio
let average x y =
  (x+.y)/.2.0
let square x =
   x*.x
let good_enough guess x = 
  equal (compare_float (Float.abs((square guess) -. x))  0.001) (-1)
let improve guess x = 
  average guess ( x /. guess)
let rec sqrt_iter guess x = 
  if (good_enough guess x) then guess 
  else (sqrt_iter (improve guess x) x)
let mysqrt x = 
  sqrt_iter 1.0 x 

let () =
  printf "sqrt: %F\n" (mysqrt 9.0)

let new_if predicate thea_clause else_clause = 
  if predicate then thea_clause
  else else_clause
let rec new_sqrt_iter guess x = 
  new_if (good_enough guess x) guess (new_sqrt_iter (improve guess x) x) 

let mynewsqrt x = 
  new_sqrt_iter 1.0 x 

let () =
  printf "sqrt: %F\n" (mynewsqrt 9.0)

(* 
sqrt: 3.00009155413
Fatal error: exception Stack overflow 

reason: Application order, else_clause will execute first. rec func cant end loop. 
*)