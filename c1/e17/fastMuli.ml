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

let rec fast_muli n m = 
  if compare m 0 = 0 then 0
  else 
    if even m then fast_muli (double n) (halve m)
    else n + (fast_muli  n (m-1))

let () = 
printf "fast muli 11 * 13 = %N\n" (fast_muli 11 13)
