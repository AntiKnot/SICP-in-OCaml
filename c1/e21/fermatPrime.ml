open Base 
open Stdio


let is_even n = if n % 2 = 0 then true else false
let is_odd n = if n % 2 = 1 then true else false

let remainder m n = 
  m % n

let square n =
  n ** 2

let rec expmod base exp m = 
  if exp = 0 then 1
  else
    if is_even exp then 
      remainder (square (expmod base (exp/2) m)) m
    else
      remainder (base * (expmod base (exp-1) m)) m

let fermat_test n = 
  let try_it = fun a -> compare (expmod a n n)  a = 0 in
    try_it (Base.Random.int(n-1)+1)

let rec fast_fermat_prime n times = 
  if times = 0 then true
  else 
    if fermat_test n then  fast_fermat_prime n (times -1)
    else false

(* let () = 
printf "fermat prime 7963 20 is %B\n" (fast_fermat_prime 7963 20);
printf "fermat prime 7159 20 is %B\n" (fast_fermat_prime 7159 20);
printf "fermat prime 7109 20 is %B\n" (fast_fermat_prime 7109 20);
printf "fermat prime 7213 20 is %B\n" (fast_fermat_prime 7213 20);
printf "fermat prime 7214 20 is %B\n" (fast_fermat_prime 7214 20); *)

let is_divides a b =
  (remainder a b) = 0

let rec find_divisor n test_divisor = 
  if square test_divisor > n then n
  else 
    if is_divides n test_divisor then test_divisor
    else find_divisor  n (test_divisor + 1)
  
let smallest_divisor n =
  (* find divisor from 2 *)
  find_divisor n 2

let () = 
printf "smallest divisor 199 is: %N\n" (smallest_divisor 199);;
printf "smallest divisor 1999 is: %N\n" (smallest_divisor 1999);;
printf "smallest divisor 19999 is: %N\n" (smallest_divisor 19999);;

(* 
runnint time in ocaml https://stackoverflow.com/questions/9061421/running-time-in-ocaml 
*)

