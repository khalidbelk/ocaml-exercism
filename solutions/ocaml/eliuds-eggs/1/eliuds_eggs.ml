let get_ascii (nb: int) : char =
  Char.chr((nb mod 2) + Char.code '0')

let rec to_char_binarylist n =
  if n = 0 then []
  else to_char_binarylist (n/2) @ [get_ascii (n mod 2)]

let to_binary n =
  if n = 0 then ['0'] else to_char_binarylist n

let egg_count number =
  number |> to_binary |> List.fold_left(fun acc ch -> if ch = '1' then acc + 1 else acc) 0