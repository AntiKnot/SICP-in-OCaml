open Base

let bigger a b c =
  List.fold_left ~f:max ~init:(a+b) [(b+c);(a+c)]
