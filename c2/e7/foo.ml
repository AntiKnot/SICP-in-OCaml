
let make_interval a b = (a, b)
let lower_bound (x, y) = 
  match (x,y) with
  (x,_) -> x
let upper_bound (x, y) = 
  match (x,y) with
  (_,y) -> y


 (* https://medium.com/arena-tech-blog/ocaml-operators-cheatsheet-d9a33bb39072 *)
 (* https://ocaml.org/manual/expr.html  *)