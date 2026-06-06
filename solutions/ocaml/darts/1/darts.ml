let is_inside (r : float) (x : float) (y : float) =
  let pow2 x = Float.pow x 2.0 in
  (pow2 x) +. (pow2 y) <= (pow2 r)                                            

type circle = Outer | Middle | Inner | Outside

let score_of_circle = function
  | Outer -> 1
  | Middle -> 5
  | Inner -> 10
  | Outside -> 0

let score (x: float) (y: float): int =
  let result =
    match (x, y) with
      | _ when is_inside 1.0 x y -> Inner
      | _ when is_inside 5.0 x y -> Middle
      | _ when is_inside 10.0 x y -> Outer
      | _ -> Outside
  in score_of_circle result
