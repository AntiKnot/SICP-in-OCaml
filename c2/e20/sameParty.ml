open Base
open Stdio

let compose f g =  
  fun x -> f (g x)
let is_even n =  
  n % 2 = 0
let is_odd  =
  compose not is_even

let rec filter precdicate l = 
  match l with
  | [] -> []
  | x::xs -> 
    if precdicate x 
    then x:: filter precdicate xs
    else filter precdicate xs

let same_party l = 
  match l with
  | [] ->[]
  | [_] ->[]
  | x::xs->let pred = if is_even x then is_even else is_odd in
  filter pred xs


let rec print_list = function 
  [] -> ()
  | e::l -> printf "%N" e ; print_string " " ; print_list l 

let () = 
  print_list (same_party [1;2;3;4;5;6;7;8;9;10]);;printf "\n";;
  print_list (same_party [2;3;4;5;6;7;8;9;10]);;printf "\n";;
