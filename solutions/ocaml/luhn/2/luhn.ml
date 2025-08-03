let normalize (str: string) : string =
  str |> String.trim |> String.to_seq |> Seq.filter (fun ch -> ch != ' ' ) |> String.of_seq

let is_only_digits (str: string) : bool =
  str |> String.trim |> String.for_all (fun ch -> ch >= '0' && ch <= '9')

let explode str =
  List.init (String.length str) (String.get str) (* Convert string to a list of chars *) 

let str_to_intlist (str: string) : int list =
  str |> String.trim |> explode |> List.filter (fun ch -> ch != ' ') |> List.map (fun ch -> int_of_char ch - int_of_char '0') (* convert to equivalent int *)

let double_but_luhn (digit: int) : int =
  let result = (digit * 2) in
  if result > 9 then (result - 9) else result

let get_luhn (cardnumber: string) : int =
  let digits = str_to_intlist cardnumber in (* Convert the cardnumber string to list of ints *)
  let len = List.length digits in
    digits
    |> List.mapi (fun i x -> if (len - i) mod 2 = 0 then double_but_luhn x else x)  (* apply the luhn doubling to the digits lists *)
    |> List.fold_left (+) 0       (* sum all the numbers of the list *)

let valid (cardnumber: string) : bool =
  let normalized = normalize cardnumber in
  match normalized with
    | cn when cn = "" || cn = "0" -> false
    | cn when not (is_only_digits cn) -> false    (* if normalized contains non-digit characters *)
    | cn when (get_luhn cn) mod 10 = 0 -> true
    | _ -> false