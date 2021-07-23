open Stdio

let reverse lst = 
  let rec rev_acc acc l=  
  match l with
  | [] -> acc
  | hd::tl -> rev_acc (hd::acc) tl
  in rev_acc [] lst

let rec print_list = function 
  [] -> ()
  | e::l -> printf "%N" e ; print_string " " ; print_list l 

let () = 
  print_list (reverse [1;2;3;4]);;
  printf "\n";; 