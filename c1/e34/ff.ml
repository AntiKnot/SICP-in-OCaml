open Base
open Stdio

let f g = (g 2);;
let square x = x*x;;

let () = 
printf "%N\n" (f square);;
printf "%N\n" (f (fun x -> (x+1)*x))

(* 
─( 21:38:25 )─< command 0 >──────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let f g = (g 2);;
val f : (int -> 'a) -> 'a = <fun>
─( 21:38:27 )─< command 1 >──────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let square x = x*x;;
val square : int -> int = <fun>
─( 21:38:34 )─< command 2 >──────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # f square;;
- : int = 4
─( 21:38:47 )─< command 3 >──────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # f f;;
Line 1, characters 2-3:
Error: This expression has type (int -> 'a) -> 'a
       but an expression was expected of type int -> 'b
       Type int -> 'a is not compatible with type int
─( 21:38:55 )─< command 4 >──────────────────────────────────────────────────────────────────────────────{ counter: 0 }─(─( 21:38:58 )─< command 4 >───────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop #

f f;;
f g 2
fun g -> (g 2) 2
 *)