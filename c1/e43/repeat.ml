open Base
open Stdio

let square x = 
  x * x


let compose f g =  
  fun x -> f (g x)

let rec repeated f n = 
  if n = 1 then f
  else repeated (compose f f) (n-1)

let () = 
printf "%N\n" ((repeated square 2) 5);;
