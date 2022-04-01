open Stdio

exception Ops of string;;

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
(* [1;2;3] -> [3;2;1] *)
let reverse nl = 
  let rec rev_acc acc nl = 
    match nl with
    | Node [] -> acc
    | Node _ -> rev_acc (cons (car nl) acc) (cdr nl) 
    | Leaf _ -> raise (Ops "append on 'a Leaf ") in
    rev_acc (Node []) nl 

let rec deep_reverse nl = 
  let rec rev_acc acc nl = 
    match nl with
    | Node [] -> acc
    | Node _ -> rev_acc (cons (deep_reverse(car nl)) acc) (cdr nl) 
    | Leaf _ -> nl in
    rev_acc (Node []) nl 

let foo = 
  reverse (Node [Leaf 1;Leaf 2;Leaf 3])

let test_foo =
  [value(car(foo));value(car(cdr(foo)));value(car(cdr(cdr(foo))))]=[3;2;1]

let bar = 
  deep_reverse (Node [Node [Leaf 1;Leaf 2];Node [Leaf 3;Leaf 4]])

let test_bar = 
  [
    (
      value(car(car(bar))),
      value(car(cdr(car(bar))))
    ),
    (
      value(car(car(cdr(bar)))),
      value(car(cdr(car(cdr(bar)))))
    )
    ] = [(4,3),(2,1)]

let () = 
  printf "test_foo is %B\n" (test_foo);;
  printf "test_bar is %B\n" (test_bar);;

let () =
  printf "%N\n" (value(car(car(bar))));;
  printf "%N\n" (value(car(cdr(car(bar)))));;
  printf "%N\n" (value(car(car(cdr(bar)))));;
  printf "%N\n" (value(car(cdr(car(cdr(bar))))));;

exception Ops of string;;
type 'a nestedlist =
  | A of 'a
  | L of 'a nestedlist list;;

let rec append nl1 nl2 =
  match (nl1,nl2) with
  | (L xs1, L xs2) -> L (xs1 @ xs2)
  | _ -> raise (Ops "append failed");;

let rec cons x xs =
  match xs with
  | L xs -> L (x::xs)
  | _ -> raise (Ops "cons failed");;

let reverse nl = 
  let rec rev_acc acc nl = 
    match nl with
    | L [] -> acc
    | L (hd::tl) -> rev_acc (cons (rev_acc (L []) hd) acc) (L tl)
    | A x -> A x in
    rev_acc (L []) nl

let nl = L [A 1;L [A 2;L [A 3;L [A 4;L [A 5]]]]];;
reverse nl;;
