type t = int * float

let make_money countryindex amount = 
  (countryindex, amount)

let get_country_idx money =
  fst money

let get_amount money =
  snd money