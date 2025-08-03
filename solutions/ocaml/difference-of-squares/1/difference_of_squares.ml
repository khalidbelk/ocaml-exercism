let square_of_sum number =
    let sum = List.fold_left (+) 0 (List.init number (fun i -> i + 1)) in
    sum * sum

let sum_of_squares number =
    let sum = List.fold_left (+) 0 (List.init number (fun i -> (i+1) * (i+1))) in
    sum
    (* (i+1) * (i+1) is equivalent to (i + 1)² *)

let difference_of_squares nb =
    let squareofsum = square_of_sum nb in
    let sumofsquares = sum_of_squares nb in
    let result = squareofsum - sumofsquares in
    result
