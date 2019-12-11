(** This module represent the weapon and how many weapons the players
    could buy in the game. *)

(**The abstract type represent the armory.*)
type t 

(**[make_armory] makes the armory with predifined weapon types and 
   amounts *)
val make_armory: t

(**[remove_weapon a weapon] returns a modified armory with  [weapon]
   removed. This happens when the player buys a weapon. Modifies [a]
   and returns a new armory*)
val remove_weapon: t->Weapon.t -> t

(**[armory_get_weapon a] returns a random weapon that exists in [a]. 
   This determines which weapon the player will get*)
val armory_get_weapon: t -> Weapon.t 