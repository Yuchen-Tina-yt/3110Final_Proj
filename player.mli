(** The abstract type of values representing the player *)
type t

(** [make_player name curr_pos money items] is the player who has the 
    attributes name name, curr_pos curr_pos, money money, and items items *)
val make_player: string -> int -> float -> Weapon.t list->int ->t


(**[change_wealth player amt] is the player after changing the player's wealth.*)
val change_wealth: t -> float -> t



(**[add_item player item] is the player after adding an item to the player's 
   items list *)
val buy_weapon: t -> Weapon.t -> t

(**[remove_item player item] is the player after deleting an item 
   from the player;s items list *)
val remove_weapon: t -> Weapon.t -> t

val move_player': t-> int -> t 

val get_id: t-> int 

val get_curr_pos: t -> int

val get_player_name: t -> string

val get_player_money: t -> float

val get_weapons: t-> Weapon.t list 

(*
(**[move_pos player steps places] is the player after moving number of steps 
   of steps from the player's current position on the map *)
val move_pos: t -> int -> Places.places_lst -> t
*)


