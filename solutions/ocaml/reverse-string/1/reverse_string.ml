(* Recursive way *)

let strlen str =
  String.length str

let rec reverse_string str =
  if String.length str = 0
    then ""
  else
    String.sub str (strlen str - 1) 1 (* extract last char *)
    ^ reverse_string ( String.sub str 0 (strlen str - 1) )(* append to next func call result, param *)
