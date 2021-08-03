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
  | Node [_] -> Node []
  | Node xs -> Node (List.tl xs)
  | _ -> raise (Ops "Unhandle.")

let value l  =  
  match l with
  | Leaf x -> x
  | _ -> raise (Ops "Leaf with out value")


let append nt1 nt2 = 
  match (nt1,nt2) with
  | (Node xs1,Node xs2) -> Node (xs1@xs2)
  | (_, Leaf _) -> raise (Ops "append on 'a -> 'a Leaf" )
  | (Leaf _,_) ->raise (Ops "append on 'a Leaf -> 'a" )


let foo = 
let l1 = Node [Leaf 1] in
let l2 = Node [Leaf 2] in
  append l1 l2

let test_foo = 
  (value(car (foo))) = 1


let cons lf nl = 
  match nl with 
  | Node [] -> Node [lf]
  | Node xs -> Node (List.cons lf xs)
  | Leaf _ -> raise (Ops "Params type error cant cons a' Leaf to a' Leaf.")


let zoo = 
  let nl1 = Leaf 2 in
  let nl2 = Node [Leaf 1] in
  cons nl1 nl2
let test_zoo = 
  (value(car(cdr (zoo)))) = 1

let mylist lf1 lf2 = 
  Node [lf1;lf2]

let baba = 
  mylist (Leaf 1) (Leaf 2)
  
let test_mylist = 
  value(car(cdr (baba))) = 2

let () = 
printf "%B\n" (test_foo);;
printf "%B\n" (test_zoo);;
printf "%B\n" (test_mylist);;