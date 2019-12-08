(**  
   This module represents the data stored for a country in the game. 
*)

(** The abstract type of values representing countries. *)
type t

(** [make_country crncy ex_rate ex_fee] is the country with currency [crncy], 
    exchange rate [ex_rate], exchange fee [ex_fee]. *)
val make_country : string -> float -> float -> t

(** [currency_in country] is the name of the currency used in [country]. *)
val currency_in : t -> string

(** [exchange_amount_for_USD country usd_amount] is the amount of money received 
    after exchanging [usd_amount] to the currency in [country]. 
*)
val exchange_amount_for_USD : t -> float -> float

(** [exchange_amount_to_USD country other_amount] is the amount of money received 
    after exchanging [other_amount] to USD. 
*)
val exchange_amount_to_USD : t -> float -> float

(** [exchange_fee_for country amount] is the amount of money lost to the bank 
    after exchanging [amount] to the local currency (doesn't matter what 
    currency [amount] is originally in).
*)
val exchange_fee_for : t -> float -> float