open Base

let is_nucleotide ch : bool =
  Char.equal ch 'A' ||  Char.equal ch 'T' || Char.equal ch 'G' ||  Char.equal ch 'C'

let empty_map = Map.empty (module Char)

let count_nucleotide (str: string) (nuc: char) : (int, char) Result.t =
  if not (is_nucleotide nuc) then
    Error nuc
  else
    match String.find str ~f:(fun ch -> not (is_nucleotide ch)) with
      | None -> Ok (String.count str ~f:(Char.equal nuc))
      | Some c -> Error c

(* Count the nucleotides in the string. *)
let count_nucleotides (str: string) =
  String.fold_result str ~init:empty_map ~f:(fun map ch ->
    match count_nucleotide (String.make 1 ch) ch with
      | Ok count -> Ok (Map.update map ch ~f:(function
                      | None -> count
                      | Some existing -> existing + count )
                    )
      | Error ch -> Error ch
  )