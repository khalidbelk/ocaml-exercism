let find arr n =
  let rec helper first last =
    if first > last then Error "value not in array"
    else
      let middle_index = (first + last) / 2 in
      let middle_value = arr.(middle_index) in
      match Int.compare middle_value n with
        | 0 -> Ok (middle_index)
        | i when middle_value < n ->
          helper (middle_index + 1) last 
        | i when middle_value > n ->
          helper first (middle_index - 1)
  in
    helper 0 (Array.length arr - 1)