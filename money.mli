(**This module represents the money in the game.*)

(** The abstract type of values representing the money *)
type t

(**[make_money countryindex amount] is the money of [amount] in the currency of 
   the country with [country_index]  *)
val make_money: int -> float -> t

(**[get_country_idx money] is the country index for the currency in [money]. *)
val get_country_idx: t -> int

(**[get_amount money] is the amount of [money] *)
val get_amount: t -> float