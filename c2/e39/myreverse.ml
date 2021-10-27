let rec fold_right f acc sequence =
  match sequence with
  | [] -> acc
  | x::xs -> f x (fold_right f acc xs)

let fold_right op initial sequence =
  let rec iter result rest = 
    match rest with
    | [] -> result
    | x::xs -> op x (iter result xs)
  in 
  iter initial sequence;;

let fold_left op initial sequence =
  let rec iter result rest = 
    match rest with
    | [] -> result
    | x::xs -> iter (op result x) xs 
  in
  iter initial sequence;;
let div x y =
  x / y;;
let r_reverse sequence =
  let helper x y =
    y@[x] in
  fold_right helper [] sequence;;

let l_reverse sequence =
  let helper x y =
    y::x in
  fold_left helper [] sequence;;
