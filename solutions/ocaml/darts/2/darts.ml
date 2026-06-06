let score (x: float) (y: float): int =
  let is_inside r x y = (Float.pow x 2.0) +. (Float.pow y 2.0) <= (Float.pow r 2.0) in   
  match (x, y) with
    | _ when is_inside 1.0 x y -> 10
    | _ when is_inside 5.0 x y -> 5
    | _ when is_inside 10.0 x y -> 1
    | _ -> 0