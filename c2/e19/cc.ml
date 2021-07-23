open Base
open Stdio

let first_denomination coin_values =
  match coin_values with
  | [] -> 0
  | hd::_ -> hd 

let except_first_denomination coin_values = 
  match coin_values with
  | [] -> []
  | _::xs -> xs

let no_more coin_values = 
  match coin_values with
  | [] -> true
  | _ -> false
 
let rec cc amount coin_values = 
  if amount = 0 then 1
  else
    if amount <0 || (no_more coin_values) then 0
    else 
      (cc amount (except_first_denomination coin_values)) + 
      (cc (amount - (first_denomination coin_values)) coin_values) 

let () = 
printf "%N\n" (cc 100 [50;25;10;5;1]);;