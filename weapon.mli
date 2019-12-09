(**This module represents items in the game*)
type t = {weapon_type: string; power: int}

(**constructor of items *)
val make_weapon:  string -> int ->t

val get_weapon_type: t -> string

val get_power: t -> int 

