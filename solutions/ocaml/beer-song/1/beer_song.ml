let get_verse (number: int) : string = 
  match number with
  | 0 -> "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall."
  | 1 -> "1 bottle of beer on the wall, 1 bottle of beer.\nTake it down and pass it around, no more bottles of beer on the wall."
  | 2 -> "2 bottles of beer on the wall, 2 bottles of beer.\nTake one down and pass it around, 1 bottle of beer on the wall."
  | n -> Printf.sprintf "%d bottles of beer on the wall, %d bottles of beer.\nTake one down and pass it around, %d bottles of beer on the wall." number number (number - 1)

let rec recite (from: int) (until: int) : string =
  match until with
  | 1 -> get_verse from
  | n -> get_verse from ^ "\n\n" ^ (recite (from-1) (until-1))