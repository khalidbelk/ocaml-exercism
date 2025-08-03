open Base
open Char
open String

let normalize str : string =
  str
  |> String.lowercase
  |> String.to_list
  |> List.sort ~compare:Char.compare (* sort the chars in ascending order *)
  |> String.of_char_list

let is_anagram str1 str2 : bool =
  String.lowercase str1 <> String.lowercase str2 && (* if same word, is not an anagram*)
  normalize str1 = normalize str2

let anagrams str strlist : string list =
  List.filter ~f:(fun s -> is_anagram str s) strlist