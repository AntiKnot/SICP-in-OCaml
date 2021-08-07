open Stdio

exception Ops of string 

type 'a tree =
  | Leaf 
  | Node of 'a * 'a tree  * 'a tree

(* let rec accumulate f acc sequence =
  match sequence with
  | [] -> acc
  | x::xs -> f x (accumulate f acc xs)  *)

let rec accumulate f acc t =
    match t with
    | Leaf -> acc
    | Node(x,l,r) -> f x (accumulate f acc l) (accumulate f acc r)

let one _ = 1

let count_leaves t =
  accumulate (fun x y z -> one(x)+y+z) 0 t

  let t = 
    Node(4,
      Node(2,
        Node(1,Leaf,Leaf),
        Node(3,Leaf,Leaf)
      ),
      Node(5,
        Node(6,Leaf,Leaf),
        Node(7,Leaf,Leaf)
      )
    )
let ()  = 
printf "%N\n" (count_leaves t) 