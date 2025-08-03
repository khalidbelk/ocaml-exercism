open Base

let empty = Map.empty (module Char)

let is_valid_nucleotide letter = 
  Char.equal 'A' letter || Char.equal 'G' letter || 
  Char.equal 'C' letter  || (Char.equal 'T' letter) 

(* Count the number of times a nucleotide occurs in the string. *)
let count_nucleotide str nucl =
  if not (is_valid_nucleotide nucl) then
    Error nucl
  else
    String.fold_result str ~init:0 ~f:(fun acc -> function
      | ch when not (is_valid_nucleotide ch) -> Error ch
      | ch when Char.equal nucl ch -> Ok (acc + 1)
      | _ -> Ok acc
    )
(* Ok (String.fold_left (fun count c -> if Char.equal c nucl then count + 1 else count) 0 str) *)

(* val count_nucleotides : string -> (int Map.M(Char).t, char) Result.t *)
let count_nucleotides str =
    String.fold_result str ~init:empty ~f:(fun acc -> function
      |  ch when not (is_valid_nucleotide ch) -> Error ch
      |  ch -> let count = Option.value ~default:0 (Map.find acc ch) in
          Ok (Map.set acc ~key:ch ~data:(count+1))
    )