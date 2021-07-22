open Base

module type PLANE =
sig
  type point
  exception BadPoint
  val make_point : int * int -> point
  val x_point : point -> int
  val y_point : point -> int

  type segment
  exception BadSeg

  (* Erroe message: Unbound type constructor point *)
  val make_segment : point * point -> segment
  val start_segment : segment -> point 
  val end_segment : segment -> point
  (* Erroe message: Unbound type constructor point *)
  val midpoint_sgement : segment -> point
end ;;

module Plane : PLANE =
struct
  type point  = Pt of int*int
  exception BadPoint
  let make_point (x,y) = 
    Pt(x,y)
  let x_point p = 
    match p with
    Pt(x,_) -> x
  let y_point p= 
    match p with
    Pt(_,y) -> y

  type segment  = Seg of point *point
  exception BadSeg
  let make_segment (p1,p2) =
    Seg(p1,p2)
  let start_segment s = 
    match s with
    Seg(p1,_) -> p1
  
  let end_segment s =
    match s with
    Seg(_,p2) -> p2
  
  let midpoint_sgement s = 
    match s with
    (* Not for float type *)
    Seg(p1,p2) -> make_point((x_point p1 + x_point p2)/2,(y_point p1 + y_point p2)/2)
end;;