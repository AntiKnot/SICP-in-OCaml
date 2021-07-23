open Base

let rec power m n =
  match  n with
  | 1 -> m
  | _ -> m * (power m (n-1))

let cons a b =
  (power 2 a) * (power 3 b)

let remainder m n = 
  m % n 
let rec car z = 
  let r =  remainder z 2 in
  match r with
  | 0 ->  1 +  (car (z/2))
  | _ -> 0
  
let rec cdr z =
  let r = remainder z 3 in
  match r with
  | 0 -> 1 + (cdr (z/3))
  | _ -> 0


