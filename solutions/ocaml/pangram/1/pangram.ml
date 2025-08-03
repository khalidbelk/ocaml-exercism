open Base

let empty_set = Set.empty (module Char)

let is_pangram str : bool =
  let char_set =
    str
      |> String.lowercase (* normalize *)
      |> String.to_list
      |> List.filter ~f:(fun ch -> Char.compare ch 'a' >= 0 && Char.compare ch 'z' <= 0)   (* remove chars not in the alphabet *)
      |> List.fold_left ~init:empty_set ~f:(fun acc ch -> Set.add acc ch)   (* build set of unique chars *)
  in
    Set.length char_set = 26       (* verify the length is equal to 26, as the alphabet length *)