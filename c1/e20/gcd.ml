open Base
open Stdio

let remainder m n = 
  m % n
let rec gcd a b =
  if compare b 0 = 0 then a 
  else gcd b (remainder a b)

let () = 
  printf "gcd 206 40 is: %N\n" (gcd 206 40)