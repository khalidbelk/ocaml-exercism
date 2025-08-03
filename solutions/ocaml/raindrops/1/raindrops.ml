let raindrop number =
  let soundString =
    (if number mod 3 = 0 then "Pling" else "") ^
    (if number mod 5 = 0 then "Plang" else "") ^
    (if number mod 7 = 0 then "Plong" else "") in

    if soundString = "" then
      string_of_int number
    else soundString