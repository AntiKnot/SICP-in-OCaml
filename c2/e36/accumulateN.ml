open Stdio
exception Ops of string

type 'a nestedlist = Leaf of 'a
                   | Node of 'a nestedlist list
let value l  =  
  match l with
  | Leaf x -> x
  | _ -> raise (Ops "Leaf with out value")


let car lst =
  match lst with
  | Node xs-> List.hd xs
  | Leaf _ -> raise (Ops "car element")

let cdr nst =
  match nst with
  | Node xs -> Node (List.tl xs)
  | Leaf _ -> raise (Ops "cdr element")
(* 
Node [
  Node [Leaf 1; Leaf 2; Leaf 3] ; 
  Node [Leaf 4; Leaf 5; Leaf 6]
  ]
-> Node [Leaf 3; Leaf 7; Leaf 9]

add (Leaf 1) (Leaf 2) -> Leaf 3
accumulate_add (Leaf 1) (Leaf 2) (Leaf 3) -> Leaf 6

enumeration [] -> next()
accumuate f initial acc list -> acc
map [] fun -> []
filter [] predicate -> []

apppend []@[]
cons x::xs

if add Node [Leaf 1]  Node [Leaf2] -> Node [Leaf 3]
  you need append 

if add Leaf 1 Leaf  2 -> Leaf 3
  you need cons

 *)
let plus x y =
  x + y

let add lf1 lf2 = 
  match lf1,lf2 with
  | Leaf x, Leaf y -> Leaf(plus x y) 
  | _ -> raise (Ops "add")

let cons lf nl = 
match nl with 
| Node xs -> Node (List.cons lf xs)
| Leaf _ -> raise (Ops "Params type error cant cons a' Leaf to a' Leaf.")

let rec foo n1 n2= 
match n1,n2 with
| Node [],Node []->Node []
| Node _,Node _-> cons (add (car n1) (car n2))  (foo (cdr(n1)) (cdr(n2)))
| _ -> raise (Ops "foo")

let rec accumulate f acc sequence =
  match sequence with
  | Node [] -> acc
  | Node xs -> f (List.hd xs) (accumulate f acc (Node (List.tl xs)))
  | _  -> raise (Ops "accumulate")

let accumulate_n nst=
  (* accumulate foo (Node []) nst 
  Node [] 卡住了我半天 init没有匹配正确。
  *)
  accumulate foo (Node [Leaf 0;Leaf 0;Leaf 0]) nst

let rec equal_tree t1 t2 =
  match (t1,t2) with
  | (Leaf x1,Leaf x2) -> x1 == x2
  | (Node [],Node [])-> true
  | (Node _, Node _) -> (equal_tree (car t1) (car t2)) && equal_tree (cdr t1) (cdr t2) 
  | _ -> false 

let test_car = 
  let result = car (Node [Leaf 1;]) in
  let expect = Leaf 1 in
  equal_tree result expect
let () = printf "test car: %B\n" (test_car)
let test1_cdr = 
  let result = cdr (Node [Leaf 1;Leaf 2]) in
  let expect = Node [Leaf 2] in
  equal_tree result expect
let () = printf "test1 cdr: %B\n" (test1_cdr)
let test2_cdr = 
  let result = cdr (Node [Leaf 2]) in
  let expect = Node [] in
  equal_tree result expect
let () = printf "test2 cdr: %B\n" (test2_cdr)
let test_add = 
  let result = add (Leaf 1) (Leaf 2) in
  let expect =  Leaf 3 in
  equal_tree result expect
let () = printf "Test add: %B\n" (test_add)

let test_cons = 
  let result = cons (Leaf 1) (Node [Leaf 2]) in
  let expect = Node [Leaf 1;Leaf 2] in
  equal_tree result expect
let () = printf "Test cons: %B\n" (test_cons)

let test_accumulate_n = 
  let result = accumulate_n (Node [
  Node [Leaf 1; Leaf 2; Leaf 3] ; 
  Node [Leaf 1; Leaf 2; Leaf 3] ; 
  Node [Leaf 1; Leaf 2; Leaf 3] ; 
  ]) in 
  let expect = (Node [Leaf 3; Leaf 6; Leaf 9]) in
  equal_tree result expect
let () = printf "Test ccumulate_n: %B\n" (test_accumulate_n)