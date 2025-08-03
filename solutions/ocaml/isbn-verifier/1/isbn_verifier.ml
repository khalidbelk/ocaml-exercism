let clean charlist =
   let filtered = List.filter (fun x -> x <> '-') charlist in
   filtered

let char_to_digit ch =
  (int_of_char ch) - (int_of_char '0')

let rec last_elem = function
  | [] -> None
  | [x] -> Some x
  | _::xs -> last_elem xs

let validate list =
  let last_element = last_elem list in
  match list with
  | _ when (List.exists ((=) 'X') list) && (last_element <> Some 'X') -> false (* If X appears but isn't used as check digit *) 
  | _ when (List.length (clean list) <> 10) -> false                           (* If after cleaning, there isn't 10 chars *) 
  | _ -> true

let compute charlist =
  let nb_steps = 10 in
  let digitlist =
    charlist |> List.map (fun c ->
      if c = 'X' then 10
      else char_to_digit c
    )
  in
  let (final_acc, _) =
    List.fold_left 
      (fun (acc, counter) elem ->
        (acc + (elem * counter), counter - 1)
      )
     (0, nb_steps) digitlist
  in
    final_acc

let is_valid str =
  let char_list = List.init (String.length str) (String.get str) in
  if not (validate char_list) then
    false
  else
    let cleaned = clean char_list in
    let computed = compute cleaned in
    computed mod 11 = 0
