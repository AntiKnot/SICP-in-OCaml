open Base
(*  choice function *)
let a_plus_abs_b a b =
  let f = if b >0 then (+) else (-) in f a b 
