
let transform (legacy_list : (int * char list) list) : (char * int) list =
  legacy_list
    |> List.map (fun (points, charlist) ->
      List.map (fun c -> (Char.lowercase_ascii c, points)) charlist
    )                 (* ex:  from [(1, ['A'; 'E'; ...])] to [[('a', 1); ('e', 1); ...]] *)
    |> List.flatten 
    |> List.sort (fun (c1, _) (c2, _) -> Char.compare c1 c2) (* alphabetically sort the tuples letter to get right order in output *)
