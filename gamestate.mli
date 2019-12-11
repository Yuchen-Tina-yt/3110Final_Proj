(** This module represents the state of the game *)

(**The abstract type representing the game board *)
type t

(**[make_state] is the game board*)
val make_state : t

(**[move_player a] modifies [a] with the position of the
   current player changed*)
val move_player : t -> unit

(**[purchase a] modifies [a] by changing the ownership of a place
   to the currrent player*)
val purchase : t -> unit

(**[develop_land a] modifies [a] by developing the current place*)
val develop_land : t -> unit

(**[rent a] modifies[a] making the current player pay a 
   rent if they end up on someone's property*)
val rent : t -> bool -> unit

(**[turn a] modifies [a] by changing the current player*)
val turn : t -> unit

(**[places_arr a] returns the array of places in [a] *)
val places_arr: t -> Place.t array

(**[get_curr_player a ] returns the current player in [a]*)
val get_curr_player: t -> Player.t

(**[get_inactive_player_id a] returns the current inactive player in [a]*)
val get_inactive_players_ids: t -> int list

(**[make_current_player_inactive a] modifies [a] to make 
   the current player inactive. *)
val make_current_player_inactive: t -> unit

(**[get_curr_player_id a] returns the id of the current player in [a]*)
val get_curr_player_id: t -> int

(**[get_curr_player_id a in] returns the country in [a] at the specified [in]*)
val country_at_index: t -> int -> Country.t

(**[trasfer_places place] chances ownership of place*)
val transfer_places: t -> int -> unit

(**[get_money_list_totoal_USD_equiv t] returns the monye total as USD*)
val get_money_list_total_USD_equiv: t -> Money.t list -> float

(**[money_string t] returns the list of all money*)
val money_string: t -> Money.t list -> string

(**[pay fl state player in country] returns a player who has been paid*)
val pay: float -> t -> Player.t -> int -> Country.t -> Player.t

(**[change_player t player] changes the player turn*)
val change_player: t -> Player.t -> unit

(**[get_free_place a] uses the chance card to get a place for free*)
val get_free_place: t-> unit

(**[check_rent t] checkes the rent *)
val check_rent: t -> bool

(**[battle a] allows the current player to battle the player who owns the land
   they land on. If they lose, they have to pay the rent plus a fee. It modifies
   [a]*)
val battle: t -> unit

(**[player_get_weapon a] modifies [a] with the modified player
   who has bought a randomly choosen weapon.*)
val player_get_weapon: t -> unit

(**[name_players a] modifies [a] with modified player name .*)
val name_players: t -> unit