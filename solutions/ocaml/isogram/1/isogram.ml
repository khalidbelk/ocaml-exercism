module CharSet = Set.Make(Char)

let to_char_list (word: string) : char list =
  let len = String.length word in 
  List.init len (String.get word)

let set_of_str (word: char list) : CharSet.t =
  List.fold_left (fun acc c -> CharSet.add c acc) CharSet.empty word

let is_isogram (word : string) : bool =
  let word = String.lowercase_ascii word in     (* convertt to lowercase *)
  let chars = to_char_list word in              (* str to char list *)
  let filtered = List.filter (fun c -> c >= 'a' && c <= 'z') chars in
  let set = set_of_str filtered in (* remove duplicates by creating a set  *)
  List.length filtered = CharSet.cardinal set (* check if the number of unique chars aka set's cardinal equal list length*)
