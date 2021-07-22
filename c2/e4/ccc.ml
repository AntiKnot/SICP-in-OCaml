open Base
open Stdio

let cons x y m = m x y
let car z= z (fun p q -> p)
let cdr z= z (fun p q -> q)