open Base
open Stdio

(* !attention  (fibnonacci n-1) + (fibnonacci n-2) will overflow *)
let rec fibonacci n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> (fibonacci (n-1)) + (fibonacci (n-2))

let rec foo n =
  match n with 
  | (0|1|2) -> n
  | _ -> (foo (n-1)) + 2*(foo (n-2)) + 3*(foo(n-3))



let () = 
printf "fib 3 is: %N\n" (fibonacci 3);;
printf "foo 3 is: %N\n" (foo 3);;
