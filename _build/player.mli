(** This module represents the data for players in a game *)

(** The abstract type of values representing the player *)
type t

(** [make_player name curr_pos money items] is the player who has the 
    attributes name name, curr_pos curr_pos, money money, and items items *)
val make_player: string -> int ->int -> Weapon.t list ->t


(**[add_wealth player money] is [player] after adding to them [money].*)
val add_wealth: t -> Money.t -> t

(** [move_player player step] is [player] after moving [step] steps. *)
val move_player': t-> int -> t 

(** [get_id player] is the identifier of the [player].*)
val get_id: t-> int 

(** [get_curr_pos player] is the current position in the game of the [player].*)
val get_curr_pos: t -> int

(** [get_player_name player] is the name of the [player] .*)
val get_player_name: t -> string

(** [get_player_money player] is the amount of money the [player] holds in each 
    currency.*)
val get_player_money: t -> Money.t list

(** [get_player_money_specific_currency player num] gets 
    the amount of money for specific currency identified for [num] for
    [player].*)
val get_player_money_specific_currency: t -> int -> float

(** [change_player_chance player] is the player after changed the chance card
    inventory*)
val change_player_chance: t -> string -> t

(**[add_weapon player weapon] is the [player] after adding weapon to their
   weapon inventory. *)
val add_weapon: t -> Weapon.t -> t

(**[remove_item player item] is the player after deleting an item 
   from the player;s items list *)
val remove_weapon: t -> Weapon.t -> t

(**[remove_weapon player weapon] is [player] they has a removed [weapon] 
   from their stock. *)
val get_weapons: t-> Weapon.t list 

(**[get_player_chance player] returns the list of the chances the [player] has*)
val get_player_chance: t -> string list

(**[mutate_player_name player] returns an new player with the original [player]
   modified name*)
val mutate_player_name: t ->  string -> t