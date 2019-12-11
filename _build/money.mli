(**The module represents the money in the game and also 
   contains relevent functions*)

(** The abstract type of values representing the money *)
type t

(**[make_money countryindex amount] create a type t based off the the 
   index identifier and amount  *)
val make_money: int -> float -> t

(**[get_country_idx money] is the index idetifier of the [money]. This
   is the value that helps to connect the currency to the country *)
val get_country_idx: t -> int

(**[get_amount money] is the float value of [money] *)
val get_amount: t -> float