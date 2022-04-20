(* https://dev.realworldocaml.org/error-handling.html *)

exception Ops of string;;

type 'a tree = 
| Leaf of 'a 
| Node of (int * 'a tree) * (int * 'a tree);;

let get_left = fun t ->
  match t with
  | Node ((_,l), _) -> l
  | Leaf _ -> raise (Ops "get_left: Leaf");;

let get_right = fun t ->  
  match t with
  | Node (_, (_,r)) -> r
  | Leaf _ -> raise (Ops "get_right: Leaf");;

let rec total_weight =  fun t -> 
  match t with
  | Leaf w -> w
  | Node ((_,l), (_,r)) -> total_weight l + total_weight r;;

let rec banalce = fun t -> 
  match t with
  | Node ((l,Leaf wl),(r,Leaf wr)) -> l*wl == r*wr 
  | Node ((l,rl),(r,tr)) -> l * total_weight rl == r * total_weight tr && banalce rl && banalce tr
  | _ -> raise (Ops "banalce: not a balanced tree");;
