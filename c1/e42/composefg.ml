open Base
open Stdio

let square x = 
  x * x

let inc x = 
  x + 1

let compose f g =  
  fun x -> f (g x)

let foo x = 
  (compose square inc) x

let () = 
printf "(compose square inc) 6 is: %N\n" (foo 6);;