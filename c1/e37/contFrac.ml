open Base
open Stdio

let  cont_frac_recur n d k = 
  let rec recur i = 
    if i = k then
      (n i) /. (d i)
    else (n i) /. ((d i) +. (recur (i+1))) in
    recur 1
  
let golden_radio k =
  (cont_frac_recur (fun _ -> 1.0) (fun _ -> 1.0) k)  +. 1.


let () = 
printf "golden radio is: %F\n" (golden_radio 1000)

(* The four arithmetic operations of ocaml do not have priority. 
If you are not sure, add parentheses *)