(* Convert string to list of chars *)
let to_char_list (str: string) : char list =
  str |> String.to_seq |> List.of_seq

(* Check if all chars of a string are non alphanumeric *)
let non_alphanumeric (str: string) : bool =
  str |> to_char_list |> List.for_all (fun ch ->
    match ch with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> false
    | _ -> true
  )  

(* Verify if a char is uppercase *)
let is_char_uppercase (ch: char) : bool =
  match ch with
  | 'A' .. 'Z' -> true
  | _ -> false

(* Verify if a string ends with '?'*)
let is_question (str: string) : bool =
  let trimmed = String.trim str in
  trimmed <> "" &&
  let last_char = String.get trimmed (String.length trimmed - 1)
  in Char.equal last_char '?'

(* Verify if all letters of a string are uppercase *)
let is_yelling (str: string) : bool =
  let char_list =
    str
    |> to_char_list
    |> List.filter (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)
  in
    char_list <> [] && List.for_all is_char_uppercase char_list

let response_for (str: string) : string =
  match str with
    | s when is_yelling s && is_question s -> "Calm down, I know what I'm doing!"
    | s when is_question s -> "Sure."
    | s when is_yelling s -> "Whoa, chill out!"
    | s when non_alphanumeric s -> "Fine. Be that way!"
    | _ -> "Whatever."