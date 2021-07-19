open Base
open Stdio
let cube x = x*.x*.x
let p x  = 3.*.x-.4.*.(cube x)
let rec sine angle = if compare_float (Float.abs angle) 0.1 = (-1) then angle else p (sine (angle /. 3.0))

let () = printf "sine 1.0: %F\n" (sine 12.15)

