exception Ops of string
type 'a nestedlist =
  | Leaf of 'a
  | Node of 'a nestedlist list

let append nl lf  = 
  match nl with
  | Node [xs] -> Node [xs;lf]
  | Node [] -> Node [lf]
  | _ -> raise (Ops "Unhandle")


let cons nl1 nl2 = 
    match (nl1,nl2) with
    | (_,Node [])  -> nl1 
    | (Node xs1,Node xs2) -> Node (List.append xs1 xs2)
    | _ -> raise (Ops "Unhandle")

let reverse nl = 
  let rec rev_acc acc nl =
    match (acc,nl)with
    | (Node _, Node []) -> acc
    | (Node xs1,Node xs2) -> rev_acc (Node (List.append xs1 [List.hd xs2]))  (Node (List.tl xs2))
    | (_,_)-> raise (Ops "Undandle") 
  in rev_acc (Node []) nl

