(* https://en.wikipedia.org/wiki/Damping *)
open Base
open Stdio

let average x y  = 
(x +.y)/.2.0

let square x = 
  x *. x

let tolerance = 0.0001
let is_close_enough p1 p2  =
  (* Float.abs (p1-.p2) > tolerance type Mismatch *)
  (* |p1-p2| < tolerance *)
  compare_float (Float.abs (p1 -.p2))  tolerance = (-1)

let average_damp f = 
  fun x -> average x (f x)

let fixed_point f guess = 
  let rec tryme guess  =
    let next = f guess in
      if is_close_enough guess next then next  
      else tryme next in
    tryme guess
  
let sqrt x = 
  fixed_point (average_damp (fun y -> x/.y)) 1.0

let cube_root x =
  fixed_point (average_damp (fun y-> x/. square y)) 1.0 

let dx = 0.00001
let deriv g = 
  fun x -> ((g (x+.dx)) -. (g x))/.dx

let cube x = x*.x*.x

let () =
printf "Derivative of cube at 5 (dx=0.0001) %F\n" ((deriv cube) 5.0)

let newton_transform g =
  fun x -> x -. ((g x)/. ((deriv g)x))

let newton_method g guess =
  fixed_point (newton_transform g) guess

let sqrt2 x =
  newton_method (fun y -> square y -. x)

let cubic a b c = 
  fun x -> x*.x*.x +. a*.x*.x +. b*.x +. c

let ()= 
printf "%F\n" (newton_method (cubic 1.0 1.0 1.0) 1.0)
