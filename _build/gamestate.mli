type t

val make_state : t

val move_player : t -> unit

val purchase : t -> unit

val turn : t -> unit

val player_arr: t -> Player.t array

val places_arr: t -> Place.t array

val get_curr_player: t -> Player.t array -> Player.t