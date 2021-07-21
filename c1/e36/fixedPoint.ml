(* fixed point https://en.wikipedia.org/wiki/Fixed_point_(mathematics) *)
(* damping https://en.wikipedia.org/wiki/Damping *)
open Base
open Stdio

let is_close_enough p1 p2  =
  (* Float.abs (p1-.p2) > tolerance type Mismatch *)
  (* |p1-p2| < tolerance *)
  compare_float (Float.abs (p1 -.p2))  0.0001 = (-1)

let formula_foo x = 
  (x +.(Float.log 1000. /. Float.log x)) /. 2.
let formula_bar x = 
   Float.log 1000. /. Float.log x


let fixed_point f guess = 
  let rec tryme guess  =
    let next = f guess in
      if is_close_enough guess next then next  
      else tryme next in
    tryme guess

let fixed_point_times f guess n= 
  let rec tryme guess n =
    let next = f guess in
      if is_close_enough guess next then n  
      else tryme next (n+1) in
    tryme guess n

let phi = fixed_point formula_foo  2.0  
let phi_times = fixed_point_times formula_foo  2.0  0
let alpha = fixed_point formula_bar  2.0  
let alpha_times = fixed_point_times formula_bar  2.0 0 

let () = 
  printf "formula foo: %F\n" (phi);;
  printf "formula foo times: %N\n" (phi_times);;
  printf "formula bar: %F\n" (alpha);;
  printf "formula bar times: %N\n" (alpha_times);;
