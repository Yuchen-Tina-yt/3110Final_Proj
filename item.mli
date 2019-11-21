(**This module represents items in the game*)
type t

(**constructor of items *)
val make_item :  string -> float ->t

val get_item_name: t -> string

val get_item_value: t -> float
