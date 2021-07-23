open Base
open Stdio


(* let square_factor = 
  let square = fun x-> x * x in
  fun l -> List.map ~f:square l

let square_list factor l = 
  factor l


let rec print_list = function 
  [] -> ()
  | e::l -> printf "%N" e ; print_string " " ; print_list l 

let () = 
  print_list (square_list square_factor [1;2;3;4;5;6;7;8;9;10]);;printf "\n";; *)
let square x= 
  x * x
let square_list l =  
  let rec iter acc r = 
    match r with
    |[] -> acc
    | hd::tl -> iter (List.append acc [square hd]) tl 
  in iter [] l
  

let rec print_list = function 
[] -> ()
| e::l -> printf "%N" e ; print_string " " ; print_list l 

let () = 
  print_list (square_list  [1;2;3;4;5;6;7;8;9;10]);;printf "\n";; 