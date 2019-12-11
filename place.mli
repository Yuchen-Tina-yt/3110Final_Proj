(**This module represents the data for places in a game*)

(**The abstract type that represents the place *)
type t

(**[make_place name country value rent_payment pass_payment]
   is the place with [name], [country], [value], [rent_payment], and 
   [pass_payment]*)
val make_place: string->int->float->float->float->t 

(**[get_ownership t] is the ownerid of [t] *)
val get_ownership : t -> int

(**[change ownership t playerid] is [t] with updated ownerindex of [playerid]*)
val change_ownership : t -> int -> t 

(**[change_land_value t new_val] is [t] with updated land value of [new_val] *)
val change_land_value: t-> float-> t 

(**[change_rent t new_val] is [t] with an updated rent of [new_val] *)
val change_rent: t -> float -> t

(**[get_rent place] is the rent for [place] *)
val get_rent: t -> float

(**[get_value place] is the value of [place]  *)
val get_value: t -> float

(**[get_place_name place] is the name of [place] *)
val get_place_name: t -> string

(**[get_country place] is the country index of [place]*)
val get_country: t -> int