open Base
open Stdio

(* helper function *)
let is_even n = n % 2 = 0
let is_odd n = n % 2 = 1

(* Fixed formula *)
let product term a next b =
  let rec iter a result  = 
    if a > b then result
    else term a *. (iter (next a)) result in
  iter a 1.0

(* Numerator and denominator *)
(* 0 . 1 2 3 4 5 6 *)
(* 2 . 2 4 4 6 6 8 *)
(* - . - - - - - - *)
(* 1 . 3 3 5 5 7 7 *)

let numerator_term k = 
  if is_odd k then k+1
  else k+2
let denominator_term k = 
  if is_odd k then k+2  
  else k+1

(* There will be a loss of accuracy, 
This is not completely implemented according to the topic. *)
let fraction k =  
  (Int.to_float (numerator_term k)) /. (Int.to_float (denominator_term k))

let inc x = x+1

let pai n = 
  4.0 *. (product fraction 1 inc n)

let () = 
printf "pai is: %F\n" (pai 10000)


(* https://en.wikipedia.org/wiki/Wallis_product *)

let wallis_product n =
  let term n =
    let n = Int.to_float n in
    (2. *. n /. (2. *. n -.1.)) *. (2.*. n /. (2.*.n +.1.)) in
  2. *. (product term 1 inc n)

let () = 
printf "wallis product pai is %F\n" (wallis_product 10000)