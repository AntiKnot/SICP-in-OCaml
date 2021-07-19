open Base
open Stdio

let even n =
  if n % 2 = 0 then true
  else false
let odd n =
  if n % 2 = 1 then true
  else false

let square x = 
  x * x 

let rec expt_iter b n a =
  if n = 0 then a
  else 
    if even n  then expt_iter (square b) (n/2) a
    else expt_iter b (n-1) b*a 

let fast_expt b n = 
  expt_iter b n 1

let () = 
printf "%N\n" (fast_expt 2 0);;
printf "%N\n" (fast_expt 2 5);;
printf "%N\n" (fast_expt 2 10);;
