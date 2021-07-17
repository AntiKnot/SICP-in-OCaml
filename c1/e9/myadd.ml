open Base

let inc x =
  x+1
let dec x = 
  x - 1
let rec add1 x y = 
  if x = 0 then y
  else inc (add1 (dec x) y)

let rec add2 x y = 
  if x = 0 then y 
  else add2 (dec x) (inc y)

(* method  add1 is not, but add2, application order will effect on add2. *) 