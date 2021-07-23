open Stdio

let rec for_each l = 
  match l with
  | [] -> printf "" 
  | x::xs ->  
    begin
      printf "%N\n" x;
      for_each  xs
    end

let () = 
  for_each [1;2;3;4;5];;