(* fixed point https://en.wikipedia.org/wiki/Fixed_point_(mathematics) *)
open Base
open Stdio

let is_close_enough p1 p2  =
  (* Float.abs (p1-.p2) > tolerance type Mismatch *)
  (* |p1-p2| < tolerance *)
  compare_float (Float.abs (p1 -.p2))  0.0001 = (-1)

let formula x = 
  1. +. (1.0 /. x)

let fixed_point f guess = 
  let rec tryme guess  =
    let next = f guess in
      if is_close_enough guess next then next  
      else tryme next in
    tryme guess

let phi = fixed_point formula  1.0  

let () = 
  printf "Golden Radio is: %F as phi.\n" (phi);;
