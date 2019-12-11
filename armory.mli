(** This module represents an armory of weapons to control how many total
    weapons the players can buy in a game. *)

(**The abstract type that represents the armory.*)
type t 

(**[make_armory] makes an armory with predifined weapons and their quantities.*)
val make_armory: t

(**[armory_remove_weapon armory weapon] returns [armory] with [weapon]
   removed. 
   Raises a failure if [armory] has no more weapons. *)
val armory_remove_weapon: t->Weapon.t -> t

(**[armory_get_weapon armory] returns a random weapon that exists in [armory].
   Raises a failure if [armory] has no more weapons.*)
val armory_get_weapon: t -> Weapon.t 