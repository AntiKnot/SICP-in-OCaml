open Base

let compose f g =  
  fun x -> f (g x)

let rec repeated f n = 
  if n = 1 then f
  else repeated (compose f f) (n-1)

let dx = 0.00001
let smooth f = 
  fun x -> (f(x+.dx) +. f(x-.dx) +. f(x)) /. 3.0

let smoonth_nth f n = 
   (repeated smooth n) f 
