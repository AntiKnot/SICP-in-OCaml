open Base
open Stdio
let average x y =
  (x+.y)/.2.0
let square x =
   x*.x
let good_enough old_guess new_guess = 
  equal (compare_float (Float.abs((new_guess -. old_guess) /. old_guess)) 0.001) (-1)
let improve guess x = 
  average guess ( x /. guess)
let rec sqrt_iter guess x = 
  if (good_enough guess (improve guess x)) then (improve guess x) 
  else (sqrt_iter (improve guess x) x)
let mysqrt x = 
  sqrt_iter 1.0 x 

let () =
  printf "sqrt: %F\n" (mysqrt 0.0009)



