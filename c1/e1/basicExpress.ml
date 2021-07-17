open Base
open Stdio

let _ = 10;;
let _ = List.fold_left ~f:( + ) ~init:0 [5;3;4];;
let _ = List.reduce ~f:( + ) [5;3;4];;
let _ = List.reduce ~f:( - ) [9;1];;
let _ = List.reduce ~f:( / ) [6;2];;
let _ = ( + ) (( * ) 2 4) ((-) 4 6);;

let foo a b = 
  let x = (+) a b in
    List.reduce ~f:( + ) [a;b;x]
let bar a b = 
  ( = ) a b
let zoo a b =
  if equal  a 4 then 6 
  else if equal b 4 then List.fold_left ~f:(+) ~init:0 [6;7;a]
  else 25

let doo a b =
  ( * ) 
  (if  a > b then a 
  else if a <b then b
  else -1) 
  ((+) a  1)

let () = 
  printf "doo 3 4 is: %N\n" (doo 3 4);;


