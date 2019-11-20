(**This module represents the data for places in a game*)

(**The abstract type represented the place *)
type t = {name:string; country: int; value: float; owner: Player.t option; 
          num_houses: int; rent_payment: float option; pass_payment: float option}

(**[make_place name country value owner num_houses rent_payment pass_payment]
   makes the type t given some parameters*)
val make_place: string->int->float->Player.t option->int->float option->
  float option->t 

(**[change ownership t player] returns a new type t with an updated  owner*)
val change_ownership : t -> Player.t option-> t 

(**[change_land_value t new_val] returns a new type t with updated vale *)
val change_land_value: t-> float-> t 

val get_value: t -> float

val get_rent: t -> float

val get_country: t -> int 

val get_owner: t -> Player.t option