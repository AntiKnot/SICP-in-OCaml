open Base
open Stdio

(* e37 recur to iter  *)

let  cont_frac n d k = 
  let rec recur i = 
    if i = k then
      (n i) /. (d i)
    else (n i) /. ((d i) +. (recur (i+1))) in
    recur 1
let tan_cf x k = 
  cont_frac
  (fun i -> if i = 1 then x else x*.x*.(-1.)) 
  (fun i -> (2.*.Int.to_float i)-.1.)
  k

let () = 
printf  "tan cf 32 is: %F\n" (tan_cf 1.0 32)