open Base
open Stdio


let remainder n m = 
  n % m
let even n = if compare (remainder n 2) 0 = 0 then true else false
let odd n  = if compare (remainder n 2) 1 = 0 then true else false 
let double n = 
  n * 2
let halve n = 
  n / 2

let rec muli_iter i j k =   
  if compare i 0 = 0 then k 
  else 
    if even j then muli_iter (double i) (halve j) k
    else muli_iter i (j-1) (k+i)

let fast_muli n m =
  muli_iter n m 0

let () = 
printf "fast muli 11 * 13 = %N\n" (fast_muli 11 13);;
printf "fast muli 11 * 14 = %N\n" (fast_muli 11 14);;