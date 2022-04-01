exception Ops of string
type 'a nestedlist = 
| A of 'a
| L of 'a nestedlist list;;

let cons x xs = 
  match xs with 
  | L [] -> L [x]
  | L xs -> L (List.cons x xs)
  | _ -> raise (Ops "");;


let rec append xs1 xs2 =
  match (xs1,xs2) with
  | (L a,L b) -> L (a@b)
  | _ -> raise (Ops "");;

cons (A 1) (L [A 2]);;

append (L [A 1]) (L [A 2]);;