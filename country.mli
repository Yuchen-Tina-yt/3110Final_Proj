(**  
   This module represents the data stored for a country in the game. 
*)

(** The abstract type of values representing countries. *)
type t

(** [currency_in country] is the name of the currency used in [country]. *)
val currency_in: t -> string

(** [exchange_received_for country usd_amount] is the amount of money received 
    after exchanging [usd_amount] in terms of the currency in [country]. 
*)
val exchange_amount_for: t -> float -> float

(** [exchange_fee_for country usd_amount] is the amount of money in USDs lost to 
    the bank after exchanging [usd_amount] in terms of the currency in 
    [country].
*)
val exchange_fee_for: t -> float -> float