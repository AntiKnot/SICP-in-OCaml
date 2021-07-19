(* 
https://en.wikipedia.org/wiki/Fibonacci_number 
https://ocaml.org/api/List.html 
https://www.cl.cam.ac.uk/teaching/1920/FoundsCS/FoCS-201920-8.pdf
*)
open Base
open Stdio

let is_even n = if n % 2 = 0 then true else false
let is_odd n = if n % 2 = 1 then true else false

let rec transpose rows = 
  match rows with
  | [] :: _ -> []
  (* here using tl_exn, the reason is I cant handle the type like a' option list. *)
  | rows -> List.map ~f: List.hd_exn rows  :: transpose (List.map ~f: List.tl_exn rows)

let rec dotprod xs ys = 
  match xs,ys with
  | [],_ -> 0
  | _,[] -> 0
  | x::xs,y::ys -> x * y + dotprod xs ys

let matprod  matA matB =
  let cols = transpose matB in
    List.map ~f:(fun row -> List.map ~f:(dotprod row) cols) matA

let first mat = 
  match mat with 
  | [[a;_];_] -> a 
  | _ -> 0

let rec fib_iter a  c  n = 
  let b = [[1;1];[1;0]] in
    if compare n 0 = 0 then first(matprod a c)
    else fib_iter (matprod a b) c (n-1)

let fib  n = 
  fib_iter [[1;1];[1;0]] [[1];[0]] n

(* let () = 
  printf "fib 3 is: %N\n" (fib 3) *)


(* This is an unusable piece of code To be fixed *)
(* ------ cut line ---------------------------------------------------------- *)


let rec fast_fib_iter a b p q count = 
  if compare count  0 = 0 then b
  else
    if is_even count then 
      fast_fib_iter a b  
      (* compute p' *)
      (p**2 + q**2) 
      (* compute q' *)
      (2*p*q + q**2)
      (count /2)
    else 
      fast_fib_iter (b*q+a*q+a*p) (b*p+a*q) p q (count -1)


let fast_fib n = 
  fast_fib_iter 1 0 0 1 n

let () = 
  printf "fib 0 is: %N\n" (fast_fib 0);;
  printf "fib 1 is: %N\n" (fast_fib 1);;
  printf "fib 3 is: %N\n" (fast_fib 3);;
  printf "fib 17 is: %N\n" (fast_fib 17);;
