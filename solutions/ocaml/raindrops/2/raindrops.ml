let is_divisible_by_x nb x =
  nb mod x = 0

let raindrop nb =
  let result =  
    (if is_divisible_by_x nb 3 then "Pling" else "") ^ 
    (if is_divisible_by_x nb 5 then "Plang" else "") ^
    (if is_divisible_by_x nb 7 then "Plong" else "")
  in
  if result = "" then string_of_int nb else result
    
  
  
