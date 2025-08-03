type dna = [ `A | `C | `G | `T ]
type rna = [ `A | `C | `G | `U ]

let to_rna (dna_list: dna list) : rna list =
  let converterfunc = function
    | `A -> `U
    | `C -> `G
    | `G -> `C
    | `T -> `A
  in
    List.map converterfunc dna_list
  