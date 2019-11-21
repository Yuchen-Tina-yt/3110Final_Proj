(**This module represents the data for places in a game*)

(**The abstract type represented the place *)
type t

(**[make_place name country value rent_payment pass_payment]
   makes the type t given some parameters*)
val make_place: string->int->float->float option->float option->t 

val get_ownership : t -> int

(**[change ownership t playerindex] returns a new type t with an updated ownerindex*)
val change_ownership : t -> int -> t 

(**[change_land_value t new_val] returns a new type t with updated vale *)
val change_land_value: t-> float-> t 

val get_value: t -> float

val get_place_name: t -> string