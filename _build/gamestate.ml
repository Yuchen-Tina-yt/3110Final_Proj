open Place 
open Item 
open Player 
open Country 
open Bank 

type t = {
  mutable players: Player.t array; 
  mutable places: Place.t array;
  mutable bank: Bank.t; 
  countries: Country.t array; 
  mutable current_player: int;
}

let make_state = 
  let player1 = Player.make_player "Shoe" 0 1000. [] in 
  let player2 = Player.make_player "Car" 0 1000. [] in 
  let player3 = Player.make_player "Hat" 0 1000. [] in
  let player4 = Player.make_player "Wheelbarrow" 0 1000. [] in
  let player_array = [|player1; player2; player3; player4|] in 

  let place1 = Place.make_place "place1" 1 100. None 0 None None in 
  let place2 = Place.make_place "place2" 1 100. None 0 None None in 
  let place3 = Place.make_place "place3" 1 100. None 0 None None in 
  let place4 = Place.make_place "place4" 1 100. None 0 None None in 
  let place5 = Place.make_place "place5" 1 100. None 0 None None in 
  let place6 = Place.make_place "place6" 1 100. None 0 None None in 
  let place7 = Place.make_place "place7" 1 100. None 0 None None in 
  let place8 = Place.make_place "Place18" 1 100. None 0 None None in 
  let place9 = Place.make_place "place9" 1 100. None 0 None None in 
  let place10 = Place.make_place "place10" 1 100. None 0 None None in 
  let place11 = Place.make_place "place11" 1 100. None 0 None None in 
  let place12 = Place.make_place "place12" 1 100. None 0 None None in 
  let place13 = Place.make_place "place13" 1 100. None 0 None None in 
  let place14 = Place.make_place "place14" 1 100. None 0 None None in 
  let place15 = Place.make_place "place15" 1 100. None 0 None None in 
  let place16 = Place.make_place "place16" 1 100.  None 0 None None in 
  let place_array = [|place1; place2; place3; place4; place5; place6; place7;
                      place8; place9; place10; place11; place12; place13; 
                      place14; place15; 
                      place16|] in 

  let bank = Bank.make_bank 100000. 4000. in 

  let country_1 = Country.make_country "USD" 1. 0. in 
  let country_2 = Country.make_country "REAL" 4.22 0.01 in
  let country_3 = Country.make_country "EUR" 0.90 0.01 in
  let country_4 = Country.make_country "YUAN" 7.03 0.01 in
  let country_5 = Country.make_country "AUD" 1.47 0.01 in
  let country_6 = Country.make_country "EGY" 16.13 0.01 in
  let country_array = [|country_1; country_2; country_3; country_4; country_5; 
                        country_6|] in 
  let c_player = 0 in 

  {players = player_array; places = place_array; bank = bank; 
   countries = country_array; current_player = c_player}

let move_player state = 
  let step = (Random.int 6) + 1 in 
  let player_int = state.current_player in 
  let player = state.players.(player_int) in 
  state.players.(player_int) <- Player.move_player player step  

(** [purchase state] is the function to purchase the land
    Add foreign currency functionality. Use helper function*)
let purchase state = 
  let player_int = state.current_player in 
  let player = state.players.(player_int) in 
  let place = state.places.(player.curr_pos) in 
  let place_value = Place.get_value place in 
  let player' = Player.change_wealth player (-. place_value) in 
  let place' = Place.change_ownership place (Some player')in 
  let bank' = Bank.deposit place_value state.bank in 
  state.players.(player_int) <- player'; 
  state.bank <- bank'; 
  state.places.(player.curr_pos) <- place'

let rent state = 
  failwith ""

(**Victor
   The cost to develop land is 5% of land and increase rent by 3%*)
let develop_land state = 
  failwith ""

let turn state = 
  state.current_player <- ((state.current_player +1) mod 4 )

(**developing land and changing the land value *)


