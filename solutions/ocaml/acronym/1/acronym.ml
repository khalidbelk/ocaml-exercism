open Base
open String

let upper_char ch = Char.uppercase ch
let is_separator ch = (Char.equal ch ' ') || (Char.equal ch '-') || (Char.equal ch '_')

let acronym str =
  str
  |> String.to_list
  |> List.fold ~init:("", true) ~f:(fun (acc, last_sep) ch ->
       if last_sep && not (is_separator ch) then
         (acc ^ String.of_char (Char.uppercase ch), false)
       else
         (acc, is_separator ch))
  |> fst
