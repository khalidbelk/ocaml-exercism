type allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats

let allergen_value = function
  | Eggs          -> 1
  | Peanuts       -> 2
  | Shellfish     -> 4
  | Strawberries  -> 8
  | Tomatoes      -> 16
  | Chocolate     -> 32
  | Pollen        -> 64
  | Cats          -> 128

(* @params score     -> the allergy score
           allergen  -> the allergen to check against
   @return a boolean indicating if it's allergic to it or not
*)
let allergic_to (score: int) (allergen: allergen) : bool =
  score land (allergen_value allergen) <> 0  (* logical and, if not equal to 0 it means the bit is on the score, so allergic (true) *)

let allergies (score: int) : allergen list =
  let all_allergens = [Eggs; Peanuts; Shellfish; Strawberries; Tomatoes; Chocolate; Pollen; Cats] in
  List.filter (allergic_to score) all_allergens
  