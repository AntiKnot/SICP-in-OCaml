open Stdio

exception Ops of string

type 'a nestedlist = 
  | Leaf of 'a 
  | Node of 'a nestedlist list;;

let car lst =
  match lst with
  | Node [] -> raise (Ops "function car not enough element.")
  | Node [x]-> x
  | Node xs-> List.hd xs
  | _ -> raise (Ops "Unhandle") 

let cdr lst =
  match lst with
  | Node [] ->raise (Ops "function cdr not enough element.")
  | Node [_] ->raise (Ops "function cdr not enough element.")
  | Node xs -> Node (List.tl xs)
  | _ -> raise (Ops "Unhandle.")

let value l  =  
  match l with
  | Leaf x -> x
  | _ -> raise (Ops "Leaf with out value")

let append nl lf  = 
  match nl with
  | Node [xs] -> Node [xs;lf]
  | Node [] -> Node [lf]
  | _ -> raise (Ops "Unhandle")

let foo = 
let l1 = Node [] in
let l2 = Leaf 1 in
  append l1 l2

let test_foo = 
  (value(car (foo))) = 1

let bar = 
  let l1 = Node [Leaf 1] in
  let l2 = Leaf 2 in
  append l1 l2

let test_bar = 
  (value(car(cdr (bar)))) = 2


let cons nl1 nl2 = 
  match (nl1,nl2) with
  | (_,Node [])  -> nl1 
  | (Node [xs1],Node [xs2]) -> Node [xs1;xs2]
  | _ -> raise (Ops "Unhandle")


let zoo = 
  let nl1 = Node [Leaf 1] in
  let nl2 = Node [Leaf 2] in
  cons nl1 nl2
let test_zoo = 
  (value(car(cdr (zoo)))) = 2

let mylist lf1 lf2 = 
  Node [lf1;lf2]

let baba = 
  mylist (Leaf 1) (Leaf 2)
  
let test_mylist = 
  value(car(cdr (baba))) = 2

let () = 
printf "%B\n" (test_foo);;
printf "%B\n" (test_bar);;
printf "%B\n" (test_zoo);;
printf "%B\n" (test_mylist);;