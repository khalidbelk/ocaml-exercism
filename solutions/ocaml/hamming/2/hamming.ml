type nucleotide = A | C | G | T

let hamming_distance strand1 strand2 =
  match (strand1, strand2) with
  | ([], []) -> Ok 0
  | ([], _) -> Error "left strand must not be empty"
  | (_, []) -> Error "right strand must not be empty"
  | _ when List.length strand1 <> List.length strand2 -> Error "left and right strands must be of equal length"
  | _ -> Ok (List.fold_left2 (fun acc c1 c2 -> if c1 <> c2 then acc + 1 else acc) 0 strand1 strand2)