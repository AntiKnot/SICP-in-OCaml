(* dot product https://en.wikipedia.org/wiki/Dot_product *)

open Stdio
exception Ops of string

let rec accumulate f acc sequence =
  match sequence with
  | [] -> acc
  | x::xs -> f x (accumulate f acc xs)

(* https://en.wikipedia.org/wiki/Cons *)
let map p sequence = 
  accumulate p [] sequence

(* https://en.wikipedia.org/wiki/Transpose *)
(* let transpose m =[] *)

let car seq = 
  match seq with
  | [] -> failwith "Failure car"
  | x::_ -> x 


let cdr seq =  
  match seq with
  | [] -> failwith "Failure cdr"
  | _::xs -> xs

let cons x xs = 
  x::xs

let car_n seqs = 
  List.map car seqs

let cdr_n seqs = 
  List.map cdr seqs

let cons_n l seqs  = 
  List.map2 cons l seqs

let rec accumulate_n  op init seqs=
  match seqs with
  | [] -> []
  | _-> cons (accumulate op init (car_n seqs)) (accumulate_n op init (cdr_n seqs))

let dot_product v w= 
  (* https://stackoverflow.com/questions/32890357/how-to-iterate-over-two-lists-at-once-in-ocaml/32891477 *)
  accumulate (+) 0 (List.map2 ( * ) v w)

let matrix_x_vector m v = 
  List.map (dot_product v) m

let foo m n = 
  List.map2 (+) m n

  (* https://www.douban.com/group/topic/72239644/ *)
let rec transpose a =
  match a with
  | []::_ -> [] 
  | _ -> List.map List.hd a :: (transpose @@ List.map List.tl a)

let matrix_x_matrix m n = 
  List.map (matrix_x_vector m) n

let test_car_n = 
  let result = car_n [[1;2;3];[4;5;6];[7;8;9]] in
  let expect = [1;4;7] in
  (compare result expect) == 0
let () = printf "Test test_car_n: %B\n" (test_car_n)

let test_cdr_n = 
  let result = cdr_n [[1;2;3];[4;5;6];[7;8;9]] in
  let expect = [[2;3];[5;6];[8;9]] in
  (compare result expect) == 0
let () = printf "Test test_cdr_n: %B\n" (test_cdr_n)

let test_cons_n = 
  let result = cons_n [1;2;3] [[1];[2];[3]] in
  let expect = [[1;1];[2;2];[3;3]] in
  (compare result expect) == 0
let () = printf "Test test_cons_n: %B\n" (test_cons_n)

let test_transpose =
  let result = transpose [[1;2;3]] in
  let expect = [[1];[2];[3]] in
  (compare result expect) == 0
let () = printf  "Test test_transpose: %B\n" (test_transpose)

let test_transpose2 =
  let result = transpose [[1];[2];[3]] in
  let expect = [[1;2;3]]in
  (compare result expect) == 0
let () = printf  "Test test_transpose2: %B\n" (test_transpose2)

let test_accumulate_n =
  let result =accumulate_n (fun x y -> x + y) 0 [[1;2;3];[1;2;3];[1;2;3]]in
  let expect = [3;6;9] in
  (compare result expect) == 0
let () = printf "Test accumulate_n: %B\n" (test_accumulate_n)

let test_dot_product = 
  let result = dot_product  [1;2;3] [3;2;1] in
  let expect = 10 in
  (compare result expect) == 0
let () = printf "Test dot_product: %B\n" (test_dot_product)

let test_matrix_x_vector = 
  let result = matrix_x_vector [[1;1;1];[2;2;2]] [3;3;3] in
  let expect = [9;18] in
  (compare result expect) == 0
let () = printf "Test matrix_x_vector: %B\n" (test_matrix_x_vector)

let test_matrix_x_matrix = 
  (* https://en.wikipedia.org/wiki/Matrix_multiplication *)
  let result = matrix_x_matrix [[1;2;3];[4;5;6]] [[10;20;30];[11;21;31]] in
  let expect = [[140;146];[320;335]] in
  (compare result expect) == 0
let () = printf "Test matrix_x_matrixl: %B\n" (test_matrix_x_matrix)