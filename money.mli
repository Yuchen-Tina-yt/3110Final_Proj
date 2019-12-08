(** The abstract type of values representing the money *)
type t

val make_money: int -> float -> t

val get_country_idx: t -> int

val get_amount: t -> float