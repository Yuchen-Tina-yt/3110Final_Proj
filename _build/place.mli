(**This module represents the data for places in a game*)

(**The abstract type that represented the place *)
type t

(**[make_place name country value rent_payment pass_payment]
   makes the type t given some parameters*)
val make_place: string->int->float->float->float->t 

(**[get_ownership t] is the ownerid of the place *)
val get_ownership : t -> int

(**[change ownership t playerid] is a new type t with an updated ownerindex*)
val change_ownership : t -> int -> t 

(**[change_land_value t new_val] returns a new type t with updated vale *)
val change_land_value: t-> float-> t 

(**[change_rent t new_val] is a new t with an upated rent *)
val change_rent: t -> float -> t

(**[get_rent place] is the rent for [place] *)
val get_rent: t -> float

(**[get_value place] is the value of the [place]  *)
val get_value: t -> float

(**[get_place_name place] is the name of the [place] *)
val get_place_name: t -> string

(**[get_coutntry place] is the country the [place] belongs to*)
val get_country: t -> int