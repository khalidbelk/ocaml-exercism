let square x = x * x
let create_list n = List.init n (fun x -> x + 1)

let square_of_sum n =
  let list = create_list n in
  let sum = List.fold_left (+) 0 list in
  square sum

let sum_of_squares n =
  let list = create_list n in
  List.fold_left (+) 0 (List.map square list)

let difference_of_squares n =
  square_of_sum n - sum_of_squares n


(* We would've used this implem if the expected param was a list already *)

(* let square x = x * x       *)

(* let square_list list = List.map square *)

(*
  let square_of_sum list = 
    let sum = List.fold_left (+) 0 list in
    square sum
*)
 
(* 
  let sum_of_squares list =
    List.fold_left (+) 0 (square_list list)
*)