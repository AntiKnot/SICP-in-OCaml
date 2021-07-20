open Base
open Stdio

(* Now express the formula in the title, 
because the commonly used definition method will warn about the free variable, 
so use the `fun ->` format *)
(* Begin to involve `series` 
Can use a partial function to construct one.*)

let is_even n = if n % 2 = 0 then true else false

let simpson f a b n = 
  let h = (b -. a) /. Int.to_float n in 
  let y k = f (a +. Int.to_float k *.h) in
  let factor k = if k = 0 || k = n then 1 else if is_even k then 4 else 2 in
  let term k = Int.to_float (factor k) *. (y k) in
  let next x = x + 1 in
  let rec sum _term _a _next _b = 
    if _a > _b then 0.0 else _term _a +. (sum _term (_next _a) _next _b)  in
  (h/.3.0) *. (sum term 0 next n)

let series n = 
  let cube x = x*.x*.x in
  simpson cube 0.0 1.0 n

let () = 
printf "simpson cube 0.0 1.0 100 is: %F\n" (series 100);;
printf "simpson cube 0.0 1.0 1000 is: %F\n" (series 1000);;
printf "simpson cube 0.0 1.0 10000 is: %F\n" (series 10000);;
