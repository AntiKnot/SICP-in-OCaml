open Base
open Stdio
  
exception GaGaGa of string

let rec last_pair l = 
  match l with
  | [a] -> a
  | _::xs -> last_pair xs
  | [] -> raise (GaGaGa "list empty -- LAST-PAIR")

let () = 
printf "%N\n" (last_pair [1]);;
printf "%N\n" (last_pair [1;2;3]);;
printf "%N\n" (last_pair []);;