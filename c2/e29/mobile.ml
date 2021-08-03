(* https://dev.realworldocaml.org/error-handling.html *)
type 'a nestedlist = Leaf of 'a
                   | Node of 'a nestedlist list
let make_mobile left right = Node [left right]
let make_branch length struc = Node [Leaf length; struc]
let left_branch tree = 
  match tree with
  |Node[left;_] -> left
  |_ -> failwith "Error left branch"
let right_branch tree =
  match tree with
  | Node [_;right] -> right
  | _ -> failwith "Error right branch"
let branch_length tree =
  match tree with
  | Node [Leaf length;_] -> length
  | _ -> failwith "Error branch length"
let branch_struct tree =
  match tree with
  | Node [_;struc] -> struc
  | _ -> failwith "Error branch struct"
