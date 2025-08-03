let is_not_zero (sides: int list) : bool = (* Check if all sides are not 0 *)
  sides
  |> List.filter (fun side -> side <> 0)
  |> List.length    (* We expect the lenght of the list of numbers != 0 *)
  |> (=) 3          (* to be the same as triangle sides, so 3 *)

let is_valid_triangle a b c = (a + b > c) && (b + c > a) && (a + c > b) (* Triangle inequality check*)

let is_equilateral (a: int) (b: int) (c: int) : bool =
  let sides = [a; b; c] in
  is_not_zero sides && is_valid_triangle a b c &&
    (a = c && b = c)

let is_isosceles (a: int) (b: int) (c: int) : bool =
  let sides = [a; b; c] in
  is_not_zero sides && is_valid_triangle a b c &&
    ((a = b) || (b = c) || (a = c))

let is_scalene (a: int) (b: int) (c: int) : bool =
  let sides = [a; b; c] in
  is_not_zero sides && is_valid_triangle a b c &&
    (a <> b && b <> c && a <> c)