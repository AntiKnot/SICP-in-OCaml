open Base 
open Stdio


(*  i 0 1 2 3 4 5 6 7 8 9 10 11 12 *)
(* Ni 1 1 1 1 1 1 1 1 1 1 1  1  1  *)
(* Di 1 1 2 1 1 4 1 1 6 1 1  8  1  *)

let di = fun i -> if i % 3 = 2 then Int.to_float ((i+1)*2/3) else  1.0
let ni = fun _ -> 1.0 

let  cont_frac_recur n d k = 
  let rec recur i = 
    if i = k then
      (n i) /. (d i)
    else (n i) /. ((d i) +. (recur (i+1))) in
    recur 1

let d_euler k = 
  cont_frac_recur ni di k

let () =
printf  "%F\n" (d_euler 12);;