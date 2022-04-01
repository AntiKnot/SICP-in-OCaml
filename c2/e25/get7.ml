(* open Base *)
open Stdio

(* (1 3 (5 7) 9)
((7))
(1 (2 (3 (4 (5 (6 7)))))) *)

(* Here I did not find a convenient way to deal with nested lists *)

(* exception Ops of string
let car lst = 
  match lst with
  |[] -> raise (Ops "function car not enough element.")
  |x::_ -> x
let cdr lst = 
  match lst with
  |[] -> raise (Ops "function cdr not enough element.")
  |_::xs -> xs
let foo = 
  [1;3;[5;7];9]
let () = 
printf "(1 3 (5 7) 9)) get %N\n" (car(cdr(cdr(cdr [1;3;[5;7];9])))) *)

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


let foo = 
  Node [Leaf 1;Leaf 3;Node [Leaf 5;Leaf 7];Leaf 9]
(* cdr -> Node [Leaf 3;Node [Leaf 5;Leaf 7];Leaf 9] *)
(* cdr -> Node [Node [Leaf 5;Leaf 7];Leaf 9] *)
(* car -> Node [Leaf 5;Leaf 7] *)
(* cdr -> Node [Leaf 7] *)
(* car -> Leaf 7 *)
(* value 7 *)


let bar = 
  (* ((7)) *)
  Node [Node [Leaf 7]]

let zoo = 
  Node [Leaf 1;Node [Leaf 2;Node [Leaf 3;Node [Leaf 4;Node [Leaf 5;Node [Leaf 6;Leaf 7]]]]]]
  (* cdr ->Node [Node [Leaf 2;Node [Leaf 3;Node [Leaf 4;Node [Leaf 5;Node [Leaf 6;Leaf 7]]]]]] *)
  (* car ->Node [Leaf 2;Node [Leaf 3;Node [Leaf 4;Node [Leaf 5;Node [Leaf 6;Leaf 7]]]]] *)
  (* cdr ->Node [Node [Leaf 3;Node [Leaf 4;Node [Leaf 5;Node [Leaf 6;Leaf 7]]]]] *)
  (* car ->Node [Leaf 3;Node [Leaf 4;Node [Leaf 5;Node [Leaf 6;Leaf 7]]]] *)
  (* cdr ->Node [Node [Leaf 4;Node [Leaf 5;Node [Leaf 6;Leaf 7]]]] *)
  (* car ->Node [Leaf 4;Node [Leaf 5;Node [Leaf 6;Leaf 7]]] *)


let () = 
  printf "%N\n" (value(car(car(bar))));;
  printf "%N\n" (value(car(cdr(car(cdr(cdr(foo)))))));;
  printf "%N\n" (value(car(cdr(car(cdr(car(cdr(car(cdr(car(cdr(car(cdr(zoo))))))))))))));;


type 'a nextedlist = 
| Atom of 'a 
| List of 'a nextedlist list;;

let head xs = 
  match xs with
  | List [] -> raise (Ops "head not enough element.")
  | List (x::_) -> x
  | Atom _ ->  raise (Ops "");;

let tail xs = 
  match xs with
  | List [] -> raise (Ops "tail not enough element.")
  | List (_::xs) -> List xs
  | Atom _ -> raise (Ops "");;

let v atom = 
  match atom with
  | Atom x -> x
  | _ -> raise (Ops "Unhandle")

let nl1 =  List [Atom 1; Atom 3; List [Atom 5; Atom 7]; Atom 9];;
let () = print_int (nl1 |> tail |> tail |> head |> tail |> head |> v);;
let nl2 =  List [List [Atom 7]];;
let () = print_int (nl2 |> head |> head |> v );;
let nl3 = List [Atom 1; List[Atom 2; List[Atom 3; List [Atom 4; List [Atom 5;List [Atom 6; List [Atom 7]]]]]]];;
let () = print_int (nl3|>tail|>head|>tail|>head|>tail|>head|>tail|>head|>tail|>head|>tail|>head|>head|>v);;