let egg_count n =
  let rec count_1s n acc =
    if n = 0 then acc
    else count_1s (n / 2) (acc + (n mod 2))
  in
    count_1s n 0
