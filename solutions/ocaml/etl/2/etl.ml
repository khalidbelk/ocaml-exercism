
let transform (legacy_list : (int * char list) list) : (char * int) list =
  legacy_list
    |> List.map (fun (points, charlist) ->
      List.map (fun c -> (Char.lowercase_ascii c, points)) charlist
    )
    |> List.flatten          (* create a (letter, itspoints) list  *)
    |> List.sort (fun (c1, _) (c2, _) -> Char.compare c1 c2) (* alphabetical sort to get right order in output *)
