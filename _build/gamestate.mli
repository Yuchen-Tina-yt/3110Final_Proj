type t

val make_state : t

val move_player : t -> unit

val purchase : t -> unit

val develop_land : t -> unit

val rent : t -> unit

val turn : t -> unit

val places_arr: t -> Place.t array

val get_curr_player: t -> Player.t