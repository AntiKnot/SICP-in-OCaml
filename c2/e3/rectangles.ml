(* https://en.wikipedia.org/wiki/Rectangle *)
open Base

module type RECTANGLE =
sig
  type rectangle 
  exception BadRectangle
  (* constructor *)
  val make_rectangle: int * int -> rectangle
  (* selector *)
  val edgel : rectangle -> int 
  (* selector *)
  val edgew : rectangle -> int
  val area : rectangle -> int
  val perimeter : rectangle -> int
  val is_equal: rectangle * rectangle -> bool
end ;;
    
module Rectangle : RECTANGLE =
struct
  type rectangle  = Rect of int*int
  exception BadRectangle
  let make_rectangle (l,w) = 
    if (l = 0) || (w=0) then raise BadRectangle
    else Rect(l,w)
  let edgel r = 
    match r with  
  Rect(l,_) -> l
  let edgew r = 
    match r with
    Rect(_,w) -> w
  let area r = 
    match r with
    Rect(l,w) -> l*w
  let perimeter r = 
    match  r with
    Rect(l,w) -> 2*l + 2*w
  let is_equal (r1,r2) = 
    match (r1,r2) with
  (Rect(l1,w1),Rect(l2,w2)) -> (l1=l2 && w1=w2) || ((l1=w2) && (l2=w1))
end;;