(** This module represents the state of the game *)

(**The abstract type representing the game board *)
type t

(**[make_state] is the initial game board*)
val make_state : t

(**[move_player a] modifies [a] to have the position of the
   current player change after a random dice roll*)
val move_player : t -> unit

(**[purchase a] modifies [a] for when the current player purchases the current 
   place.
   Raises a failure if the current place is already owned. *)
val purchase : t -> unit

(**[develop_land a] modifies [a] according to when the current player develops 
   the current place. 
   Raises a failure if the current place isn't owned by the current player. *)
val develop_land : t -> unit

(**[rent a is_higher] modifies [a] to make the current player pay a rent to the 
   owner of the current place. The rent is 1.5 times more expensive if [is_higher] 
   is true. *)
val rent : t -> bool -> unit

(**[turn a] modifies [a] to have the current player changed to the next player*)
val turn : t -> unit

(**[places_arr a] is the array of places in [a] *)
val places_arr: t -> Place.t array

(**[get_curr_player a ] returns the current player in [a]*)
val get_curr_player: t -> Player.t

(**[get_inactive_player_id a] returns the current inactive player ids in [a]. *)
val get_inactive_players_ids: t -> int list

(**[make_current_player_inactive a] is [a] with the the current player made 
   inactive. *)
val make_current_player_inactive: t -> unit

(**[get_curr_player_id a] is the id of the current player in [a]*)
val get_curr_player_id: t -> int

(**[get_curr_player_id a in] returns the country in [a] at the specified index 
   [in].*)
val country_at_index: t -> int -> Country.t

(**[transfer_places t receiver] modifies [t] to give ownership of all places 
   owned by the the current player to [receiver]*)
val transfer_places: t -> int -> unit

(**[get_money_list_total_USD_equiv money_list] returns the total of 
   [money_list] in USD's*)
val get_money_list_total_USD_equiv: t -> Money.t list -> float

(**[money_string money_list] returns the output to the interface for 
   [money_list] in [t]*)
val money_string: t -> Money.t list -> string

(** [pay amount state player country_idx country] is [player] after paying 
    the [amount] in the terms of the currency in [country].*)
val pay: float -> t -> Player.t -> int -> Country.t -> Player.t

(**[change_player t player] modifies [t] to move on to the next player*)
val change_player: t -> Player.t -> unit

(**[get_free_place a] modifies [a] for when the current player 
   attempts to use a chance card to get a place for free*)
val get_free_place: t-> unit

(**[check_rent t] is true if the rent needs to be paid in [t]*)
val check_rent: t -> bool

(**[battle a] modifies [a] after the current player battles the owner of the 
   current place. *)
val battle: t -> unit

(**[player_get_weapon a] modifies [a] with the modified player
   who has bought a randomly choosen weapon.*)
val player_get_weapon: t -> unit

(**[name_players a] modifies [a] with all the modified player's names.*)
val name_players: t -> unit

(** [use_chance_card a strlist card_name] modifies [a] for when the current 
    player uses a card of [card_name] which will fail if [strlist] does not 
    represent a card of [card_name] or if the current player doesn't have that 
    card. *)
val use_chance_card: t -> string list -> string -> unit