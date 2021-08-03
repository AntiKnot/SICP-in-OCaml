open Stdio

exception Ops of string

type 'a nestedlist = Leaf of 'a
                   | Node of 'a nestedlist list

let car lst =
  match lst with
  | Node xs-> List.hd xs
  | Leaf _ -> raise (Ops "car element")

let cdr lst =
  match lst with
  | Node xs -> Node (List.tl xs)
  | Leaf _ -> raise (Ops "cdr element")

let square x= 
  x * x
let rec map_tree f t =  
    match t with
    | Leaf x -> Leaf (f x)
    | Node xs -> Node  (List.map (map_tree f) xs) 

let rec equal_tree t1 t2 =
  match (t1,t2) with
  | (Leaf x1,Leaf x2) -> x1 == x2
  | (Node [],Node [])-> true
  | (Node _, Node _) -> equal_tree (car t1) (car t2) && equal_tree (cdr t1) (cdr t2) 
  | _ -> false

let t1 = 
  Node [Node [Leaf 1];Leaf 2;Leaf 3]

let t2 = 
  Node [Node [Leaf 1];Leaf 4;Leaf 9]

let () = 
  printf "%B\n" (equal_tree (map_tree square t1) t2);;