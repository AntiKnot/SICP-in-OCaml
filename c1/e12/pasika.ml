open Base
open Stdio

(* Pascal's Triangle
https://en.wikipedia.org/wiki/Pascal%27s_triangle 
(0,0)
(1,0)(1,1)
(2,0)(2,1)(2,2)
...
*)

let rec pascal row col = 
  if col = 0 then 1
  else if row =  col then 1
  else (pascal (row-1) (col-1)) + (pascal (row-1) (col))   


let () = 
  printf "Pascal Triangle number is: %N\n" (pascal 4 2);;
