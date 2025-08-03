type planet = Mercury | Venus | Earth | Mars
            | Jupiter | Saturn | Neptune | Uranus

let to_earth_years (seconds: int) =
  float_of_int seconds /. (365.0 *. 24.0 *. 60.0 *. 60.0)

let age_on (planet: planet) (seconds: int) : float =
    match planet with
    | Earth -> to_earth_years seconds
    | Mercury -> to_earth_years seconds /. 0.2408467 
    | Venus -> to_earth_years seconds /. 0.61519726
    | Mars -> to_earth_years seconds /. 1.8808158
    | Jupiter -> to_earth_years seconds /. 11.862615
    | Saturn -> to_earth_years seconds /. 29.447498
    | Neptune -> to_earth_years seconds /. 164.79132
    | Uranus -> to_earth_years seconds /. 84.016846