(* 
(define (p) (p))

(define (test x y)
  (if (= x 0)
    0
    y))
(test 0 (p)) 

如果解释器采用的是应用序，则程序会不断地执行，因为需要求值p函数，然而p函数返回自己，因此进入死循环。
(test 0 (p)) 
(test 0 (p)) 
(test 0 (p))
...

而对于采用正则序的解释器，程序则能成功输出0，因为，解释器先展开过程，再求值(如果表达式有用到，而这里因为x=0，并没有用到p函数)，过程是这样的:
(test 0 (p)) 
 (if (= 0 0) 0 (p)) 
 (if #t 0 (p)) 
 0 
*)

(* 
not allowed define like this 

let rec foo = 
  foo 
  *)

open Base
open Stdio

(* let bar a= 
  a *)
let rec bar a = 
  bar a


let test x y =
  if x= 0 then 0 else y


let () = 
printf "result: %N\n" (test 0 (bar 0))

(* ocaml 也是应用序求值 Application Order *)