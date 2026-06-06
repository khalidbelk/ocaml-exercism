let is_valid nb = if nb > 0 then Ok (nb) else Error "Only positive integers are allowed"

let collatz_conjecture nb =
  let count = 0 in
  let valid_nb = is_valid nb in
  let rec helper nb count =
    match nb with
    | 1 -> count
    | i when (nb mod 2 = 0) -> (helper (nb/2) (count+1))
    | _ -> helper (nb * 3 + 1) (count + 1)
  in
    (*if valid_nb gives (Ok x), it will apply that fun to it, if it's an Error, it will directly throw it *)
    Result.map (fun x -> helper nb count) valid_nb
