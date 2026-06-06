type character = {
  charisma: int; constitution: int; dexterity: int;
  hitpoints: int; intelligence: int; strength: int; wisdom: int;
}

let roll_dice ()  =
  let results = List.init 4 (fun _ -> 1 + Random.int 6)
  in results |> List.sort (compare) |> List.tl

let ability () =
  let dice_result = roll_dice () in
  dice_result |> List.fold_left (+) 0

let modifier ~score =
  let res = (float_of_int(score) -. 10.0) /. 2.0 in
  int_of_float(floor res)

let generate_character () =
  let charisma = ability () in
  let constitution = ability () in
  let dexterity = ability () in
  let hitpoints = 10 + modifier ~score:constitution in
  let intelligence = ability () in
  let strength = ability () in
  let wisdom = ability () in
  { charisma; constitution; dexterity; hitpoints; intelligence; strength; wisdom }
