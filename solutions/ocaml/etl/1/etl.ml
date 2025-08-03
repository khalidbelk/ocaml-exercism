open Base

let get_points (letter: char) : int =
  match letter with
    | 'a' | 'e' | 'i' | 'o' | 'u' | 'l' | 'n' | 'r' | 's' | 't' -> 1
    | 'd' | 'g' -> 2
    | 'b' | 'c' | 'm' | 'p' -> 3
    | 'f' | 'h' | 'v' | 'w' | 'y' -> 4
    | 'k' -> 5
    | 'j' | 'x' -> 8
    | 'q' | 'z' -> 10
    | _ -> 0

let transform (legacy_list : (int * char list) list) : (char * int) list =
  legacy_list
    |> List.concat_map ~f:(fun (_, charlist) -> charlist) (* flatten list *)
    |> List.sort ~compare:Char.compare             (* alphabetical sort to get right order in output *)
    |> List.map ~f:(fun letter ->
        let lowercase = Char.lowercase letter in
        (lowercase, get_points lowercase)
      )                (* create a (letter, itspoints) list  *)