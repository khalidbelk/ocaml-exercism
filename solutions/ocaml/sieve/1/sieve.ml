(* Useful link -> https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes#Example *)

let sieve nb =
  let initial = List.init (nb - 1) (fun x -> x + 2) in    (*Create the initial list*)
  let rec sieve_it_up list =
    match list with
    | [] -> []
    | prime :: rest when prime * prime > nb -> list (* Stop sieving *)
    | prime :: rest ->
        prime :: sieve_it_up (List.filter (fun x -> x mod prime <> 0) rest) (* Filter every Xth number after X *)
  in
    sieve_it_up initial

let primes nb =
    if nb <= 0 then failwith "Input number should be greater than or equal to 1"
    else sieve nb