type t 

val make_armory: t

val remove_weapon: t->Weapon.t -> t

val armory_get_weapon: t -> Weapon.t 