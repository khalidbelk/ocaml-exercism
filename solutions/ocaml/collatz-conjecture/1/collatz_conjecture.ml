let is_even nb = nb mod 2 = 0
let is_valid nb =
  match nb with
  | i when i > 0 -> Ok (i)
  | _ -> Error "Only positive integers are allowed";;

let collatz_conjecture nb =
  let count = 0 in
  let valid_nb = is_valid nb in
  let rec helper nb count =
    match nb with
    | 1 -> count
    | i when (is_even nb) -> (helper (nb/2) (count+1))
    | _ -> helper (nb * 3 + 1) (count + 1)
  in
    match valid_nb with
      | Ok (nb) -> Ok (helper nb count)
      | Error msg -> Error msg