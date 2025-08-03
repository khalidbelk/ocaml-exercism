(* Remove whitespaces *)
let normalize (str: string) : string =
  str |> String.trim |> String.to_seq |> Seq.filter (fun ch -> ch != ' ' ) |> String.of_seq

(* Verify if all the chars of the cardnumber string are a valid digit *)
let is_only_digits (str: string) : bool =
  str |> String.trim |> String.for_all (fun ch -> ch >= '0' && ch <= '9')

(* Convert string to a list of chars *) 
let explode str = List.init (String.length str) (String.get str)

(* Convert string to a list of ints *) 
let str_to_intlist (str: string) : int list =
  str |> String.trim |> explode
  |> List.filter (fun ch -> ch != ' ') (* remove spaces in between *)
  |> List.map (fun ch -> int_of_char ch - int_of_char '0') (* convert to equivalent int *)

(* Double a number, but if result > 9 then return result - 9  *)
let double_but_luhn (digit: int) : int =
  let result = (digit * 2) in
  if result > 9 then (result - 9) else result

(* Double first and third digits of an int list - changed method so commenting it
let double_first_and_third (cardnumber: int list) : int list =
  match cardnumber with
    | first::second::third::fourth -> (double_but_luhn first)::second::(double_but_luhn third)::fourth
    | _ -> cardnumber *)

(* Double every second digit from right to left *)
let double_every_second (cardnumber: int list) : int list =
  let len = List.length cardnumber
  in
    List.mapi (fun i x -> 
      if (len - i) mod 2 = 0 then double_but_luhn x else x
    ) cardnumber

let get_luhn (cardnumber: string) : int =
  let digits = str_to_intlist cardnumber in (* Convert the cardnumber string to list of ints *)
    digits 
    |> double_every_second  (* apply the luhn doubling to every 4-digit lists *)
    |> List.fold_left (+) 0       (* sum all the numbers of the list *)

let valid (cardnumber: string) : bool =
  let normalized = normalize cardnumber in
  match normalized with
    | "" -> false
    | "0" -> false
    | cn when not (is_only_digits cn) -> false    (* if normalized contains non-digit characters *)
    | cn when (get_luhn cn) mod 10 = 0 -> true
    | _ -> false
