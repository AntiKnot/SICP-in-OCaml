open Stdio

exception Ops of string

type 'a nestedlist =
  | Leaf of 'a
  | Node of 'a nestedlist list

let value l  =  
  match l with
  | Leaf x -> x
  | _ -> raise (Ops "Leaf with out value")

let car lst =
  match lst with
  | Node xs-> List.hd xs
  | Leaf _ -> raise (Ops "car element")

let cdr lst =
  match lst with
  | Node xs -> Node (List.tl xs)
  | Leaf _ -> raise (Ops "cdr element")

let append nt1 nt2 = 
  match (nt1,nt2) with
  | (Node xs1,Node xs2) -> Node (xs1@xs2)
  | (_, Leaf _) -> raise (Ops "append on 'a -> 'a Leaf" )
  | (Leaf _,_) ->raise (Ops "append on 'a Leaf -> 'a" )

let cons lf nl = 
  match nl with 
  | Node xs -> Node (List.cons lf xs)
  | Leaf _ -> raise (Ops "Params type error cant cons a' Leaf to a' Leaf.")

let rec fringe nl = 
  let rec fringe_acc acc nl = 
  match nl with
  | Node [] ->acc
  | Node _ ->fringe_acc (List.append acc (fringe (car nl))) (cdr nl)
  | Leaf _ -> [value nl] in
  fringe_acc [] nl

let foo = 
  Node [Leaf 1;Leaf 2;Leaf 3;Leaf 4]
let test_foo = 
  fringe foo = [1;2;3;4]
let () = 
printf "%B\n" (test_foo);;
let bar = 
  Node [ Node [Leaf 1;Leaf 2];Node [Leaf 3;Leaf 4]]
let test_bar = 
  fringe bar =[1;2;3;4]
let () = 
printf "%B\n" (test_bar);;


type 'a nestedlist =
  | A of 'a
  | L of 'a nestedlist list;;

let rec cons x xs =
  match xs with
  | L xs -> L (x::xs)
  | _ -> raise (Ops "cons failed");;

let fringe nl = 
  let rec acc_fringe acc nl =
    match nl with
    | L [] -> acc
    | L (x::xs) -> acc_fringe (acc@(acc_fringe [] x)) (L xs) 
    | A x -> [x] in
    acc_fringe [] nl;;
let nl = L [A 1;L [A 2;L [A 3;L [A 4;L [A 5]]]]];;

assert ((fringe nl) = [1;2;3;4;5]);;