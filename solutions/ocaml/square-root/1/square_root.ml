(*Newton method*)

let abs n =
  if n < 0.0 then -.n else n

let newton num =
  let tolerance = 0.0001 in
  let initial_guess = if num > 1.0 then num /. 2.0 else 1.0 in
  let rec loop x =
    let root = 0.5 *. (x +. (num /. x)) in
    if abs(root -. x) < tolerance then
      root
    else
      loop root
  in
    loop initial_guess

let square_root num =
  if num < 0 then failwith "The number should be positive"
  else int_of_float (newton (float_of_int num))