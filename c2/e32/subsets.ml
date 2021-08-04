open Stdio

(* 
In the previous mode 

let foo x y = 
  let bar = 1 in
  let zoo = 2 in
  x + y + bar + zoo

In this mode

let recursive expression in the matched sub-items
*)


let rec subsets l = 
  match l with
  | [] -> [[]]
  | x::xs -> 
    let rest = subsets xs 
    in rest @ (List.map (fun y -> x::y) rest)


(* 1::[2] -> [1;2] *)
(* [1]@[2] -> [1;2] *)

(* 
Each item in the list has two states, selected or unselected.
line in rest @ (List.map (fun y -> x::y) rest) combine two states.
*)

let res = subsets [1;2;3];;
let exp = [[];[3];[2];[2;3];[1];[1;3];[1;2];[1;2;3]];;

(* 
[]
> not 3 with 3
[]  [3]
> [] not 2 with 2, [3] not 2 with 2
[] [2] , [3]  [2;3]
[2] [2;3] [1;3] [1;2] [1;2;3] 
 *)

let () = 
printf "%N\n" (compare res exp);;

(* 
─( 11:16:02 )─< command 0 >───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let rec subsets l = 
  match l with
  | [] -> [[]]
  | x::xs -> 
    let rest = subsets xs 
    in rest @ (List.map (fun y -> x::y) rest);;
val subsets : 'a list -> 'a list list = <fun>
─( 11:16:04 )─< command 1 >───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let result = subsets [1;2;3];;
val result : int list list =
  [[]; [3]; [2]; [2; 3]; [1]; [1; 3]; [1; 2]; [1; 2; 3]]
─( 11:16:09 )─< command 2 >───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let expect = [[];[3];[2];[2;3];[1];[1;3];[1;2];[1;2;3]];;
val expect : int list list =
  [[]; [3]; [2]; [2; 3]; [1]; [1; 3]; [1; 2]; [1; 2; 3]]
─( 11:16:13 )─< command 3 >───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # result == expect 
;;
- : bool = false
─( 11:16:25 )─< command 4 >───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # 
 *)

