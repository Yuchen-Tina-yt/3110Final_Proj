(**This module represents the data for places in a game*)

(**The abstract type represented the place *)
type t 

(**[make_place name country value owner num_houses rent_payment pass_payment]
   makes the type t given some parameters*)
val make_place: string->string->float->Player.t option->int->int option->int option->t 

(**[change ownership t player] returns a new type t with an updated  owner*)
val change_ownership : t -> Player.t option-> t 

(**[change_land_value t new_val] returns a new type t with updated vale *)
val change_land_value: t-> float-> t 