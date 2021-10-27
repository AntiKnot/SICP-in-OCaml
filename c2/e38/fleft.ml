let rec accumulate f acc sequence =
  match sequence with
  | [] -> acc
  | x::xs -> f x (accumulate f acc xs)

let fold_left op initial sequence =
  let  rec iter result rest = 
    match rest with
    | [] -> result
    | x::xs -> iter (op result x) xs 
  in
  iter initial sequence;;

let add x y =
  x + y;;

let div x y =
  x / y;;

(* Commutative property ? *)