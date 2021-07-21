open Base
open Stdio


let double f =
  fun g ->f (f g)

let inc x = 
  x + 1

let twice x = 
  x * 2

let foo = 
  (double (double double)) inc


let () = 
printf "%N\n" ((double inc) 1);;
printf "%N\n" ((double twice) 1);;
printf "%N\n" (foo 5);;