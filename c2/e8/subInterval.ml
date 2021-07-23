let make_interval a b = (a, b)
let lower_bound (x, y) = 
  match (x,y) with
  (x,_) -> x
let upper_bound (x, y) = 
  match (x,y) with
  (_,y) -> y

let sub_interval i1 i2 = 
  match (i1,i2) with
  ((x1,y1),(x2,y2)) -> make_interval (x1-x2,y1,y2) 