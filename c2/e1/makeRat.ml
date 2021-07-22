(* https://github.com/anwarmamat/cmsc330/blob/master/ocaml/OCaml_Lecture_4/fraction_module_sig.ml *)
open Base
(* • (make-rat ⟨n⟩ ⟨d⟩) returns the rational number whose numerator is the integer ⟨n⟩ and whose denominator is the integer ⟨d ⟩. *)
module type FRACTION =
sig
(* hide the type *)
  type fraction 
  exception BadFrac
  (* val gcd: int * int -> int *)
  (* gcd is not visiable outside the module *)
  val reduce : fraction ->fraction
  (* constructor *)
  val make_frac : int * int -> fraction
  (* selector *)
  val numer : fraction -> int 
  (* selector *)
  val denom : fraction -> int
  val add : fraction * fraction -> fraction
  val sub : fraction * fraction -> fraction
  val nul : fraction * fraction -> fraction
  val div : fraction * fraction -> fraction
  val is_equal: fraction * fraction -> bool
  val to_string: fraction -> string
end ;;
    
module Fraction : FRACTION =
struct
  type fraction  = Frac of int*int
  exception BadFrac

  let rec gcd (x,y) =
    (*let _ = print_int x; print_string "\t"; print_int y in *)
    let (x,y) = if x>=y then (x,y) else (y,x) in
    if y = 0 then x else gcd(y,x%y)
  let reduce (Frac(x,y)) = 
    (* denominator cannot be 0 *)
    let d = gcd(x,y) in 
    Frac((x/d), (y/d))  
  let make_frac (x,y) =
    if y = 0 then raise BadFrac
    else reduce(Frac(x,y))
  let numer r = 
    match r with Frac(n,_) -> n
  let denom r = 
    match r with Frac(_,d) -> d
  let add (r1,r2) = 
    match (r1,r2) with
  (Frac(n1,d1),Frac(n2,d2)) -> reduce (Frac(n1*d2 + d1*n2, d1*d2))
  let sub (r1,r2) = 
    match (r1,r2) with
  (Frac(n1,d1),Frac(n2,d2)) -> reduce (Frac(n1*d2 - d1*n2, d1*d2))
  let nul (r1,r2) =
    match (r1,r2) with
  (Frac(n1,d1),Frac(n2,d2)) -> reduce (Frac(n1*n2, d1*d2))
  let div (r1,r2) = 
    match (r1,r2) with
  (Frac(n1,d1),Frac(n2,d2)) -> reduce (Frac(n1*d2, d1*n2))
  let is_equal (r1,r2) = 
    match (r1,r2) with
  (Frac(n1,d1),Frac(n2,d2)) -> n1*d2 = n2*d1
  let to_string (Frac(a,b)) = 
    if b = 1 then Int.to_string a
    else 
      if a = 0 then "0"
      else (Int.to_string a) ^ "/" ^ (Int.to_string b)
  end;;