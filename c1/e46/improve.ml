open Base
open Stdio

let average a b =
  (a+.b) /. 2. 

let average_damp f =
  fun x -> average x (f x)

let square x =
  x *. x 
let tolerance = 0.001
let close_enough p1 p2 = 
  compare_float (Float.abs (p1-.p2)) tolerance = (-1)


let iterative_improve is_good_enough improve = 
  let rec iter guess = 
    let next = improve guess in
    if is_good_enough guess then next
    else iter next in
  fun guess -> iter guess

let sqrt x = 
  let improve guess = 
    average guess (x/.guess) in
  let is_good_enough guess = 
    let next = improve guess in
      close_enough guess next in
  (iterative_improve is_good_enough improve) 1.0

let () = 
printf "sqrt 9 is: %F\n" (sqrt 11.0)


