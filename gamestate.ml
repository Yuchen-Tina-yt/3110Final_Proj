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
  mutable inactive_players_ids: int list
}

let places_arr (state: t ) = state.places
let country_at_index state idx  = Array.get state.countries idx

let get_curr_player (state:t) : Player.t = 
  Array.get state.players state.current_player

let get_inactive_players_ids state = state.inactive_players_ids

let make_current_player_inactive state = 
  state.inactive_players_ids <- state.current_player :: state.inactive_players_ids

let make_state = 
  let player1 = Player.make_player "Shoe" 0 1000. [] 0 in 
  let player2 = Player.make_player "Car" 0 1000. [] 1 in 
  let player3 = Player.make_player "Hat" 0 1000. [] 2 in
  let player4 = Player.make_player "Wheelbarrow" 0 1000. [] 3 in
  let player_array = [|player1; player2; player3; player4|] in 

  let place1 = Place.make_place "place1" 0 100. 15. 15. in 
  let place2 = Place.make_place "place2" 1 100. 15. 15. in 
  let place3 = Place.make_place "place3" 2 100. 15. 15. in 
  let place4 = Place.make_place "place4" 3 100. 15. 15. in 
  let place5 = Place.make_place "place5" 4 100. 15. 15. in 
  let place6 = Place.make_place "place6" 5 100. 15. 15. in 
  let place7 = Place.make_place "place7" 0 100. 15. 15. in 
  let place8 = Place.make_place "Place8" 1 100. 15. 15. in 
  let place9 = Place.make_place "place9" 2 100. 15. 15. in 
  let place10 = Place.make_place "place10" 3 100. 15. 15. in 
  let place11 = Place.make_place "place11" 4 100. 15. 15. in 
  let place12 = Place.make_place "place12" 5 100. 15. 15. in 
  let place13 = Place.make_place "place13" 0 100. 15. 15. in 
  let place14 = Place.make_place "place14" 1 100. 15. 15. in 
  let place15 = Place.make_place "place15" 2 100. 15. 15. in 
  let place16 = Place.make_place "place16" 3 100. 15. 15. in 
  let place_array = [|place1; place2; place3; place4; place5; place6; place7;
                      place8; place9; place10; place11; place12; place13; 
                      place14; place15; 
                      place16|] in 

  let bank = Bank.make_bank 100000. 4000. in 

  let country_1 = Country.make_country "USD $" 1. 0. in 
  let country_2 = Country.make_country "BRL R$" 4.22 0.01 in
  let country_3 = Country.make_country "EUR €" 0.90 0.01 in
  let country_4 = Country.make_country "CNY ¥" 7.03 0.01 in
  let country_5 = Country.make_country "AUD A$" 1.47 0.01 in
  let country_6 = Country.make_country "EGP E£" 16.13 0.01 in
  let country_array = [|country_1; country_2; country_3; country_4; country_5; 
                        country_6|] in 
  let c_player = 0 in 

  {players = player_array; places = place_array; bank = bank; 
   countries = country_array; current_player = c_player; 
   inactive_players_ids = []}

let move_player state = 
  let step = (Random.int 6) + 1 in 
  let player_int = state.current_player in 
  let player = state.players.(player_int) in 
  state.players.(player_int) <- Player.move_player' player step  

(** [purchase state] is the function to purchase the land
    Add foreign currency functionality. Use helper function*)
let purchase state = 
  let player_index = state.current_player in 
  let player = state.players.(player_index) in 
  let place = state.places.(get_curr_pos player) in 
  let country = state.countries.(get_country place) in
  let place_value = Place.get_value place in
  let exchange_fee = exchange_fee_for country place_value in
  if (get_ownership place = -1) then begin
    let player' = Player.change_wealth player (-.place_value -.exchange_fee) in 
    let player_id = Player.get_id player' in
    let place' = Place.change_ownership place (player_id)in 
    let bank' = Bank.deposit 
        (place_value +. exchange_fee) state.bank in 
    state.players.(player_index) <- player'; 
    state.bank <- bank'; 
    state.places.(get_curr_pos player) <- place';
    print_endline ("Congrats, you successfully purchased this previously " ^ 
                   "unowned land, " ^ (get_place_name place') ^ "!");
    print_string "You now have USD $"; print_float (get_player_money player');
    print_endline "0.\n";
  end
  else begin
    (* Buying the place from another player. 
       Right now, the old owner 
       doesn't get a choice whetheer or not they allow the current player to buy 
       the place from them. *)
    let cur_owner_int = Place.get_ownership place in 
    let owner = state.players.(cur_owner_int) in 
    let place_1 = Place.change_ownership place player_index in 
    let player_1 = Player.change_wealth player (-.place_value -.exchange_fee) in 
    let owner' = Player.change_wealth owner (+. place_value) in 
    let bank' = Bank.deposit exchange_fee state.bank in 
    state.places.(get_curr_pos player) <- place_1; 
    state.players.(player_index)<- player_1; 
    state.players.(cur_owner_int) <- owner';
    state.bank <- bank'; 
    print_endline ("Congrats, you successfully purchased this land, " ^ 
                   (get_place_name place_1) ^", from Player " ^ 
                   (get_player_name owner') ^ "!");
    print_string "You now have USD $"; print_float (get_player_money player_1);
    print_endline "0."; print_string "Player ";
    print_string (get_player_name owner'); print_string " now has USD $";
    print_float (get_player_money owner'); print_endline "\n";
  end

let transfer_places giver receiver places=
  Array.map (fun h -> if get_ownership h = giver 
              then change_ownership h receiver
              else h) places

let rent state = 
  let curr_player_id = state.current_player in 
  let curr_player = state.players.(curr_player_id) in 
  let place = state.places.(get_curr_pos curr_player) in
  let country_idx = get_country place in 
  let country = state.countries.(country_idx) in
  let owner_id = Place.get_ownership place in
  let currency = currency_in country in
  if ((owner_id = -1)|| (owner_id = curr_player_id) ) then ()
  else begin
    let paid_player = state.players.(owner_id) in
    let rent = Place.get_rent place in
    let exchange_fee = exchange_fee_for country rent in
    print_string"You landed on Player ";
    print_string (Player.get_player_name paid_player);
    print_string ("'s place. You paid a rent of " ^ currency);
    print_float (exchange_amount_for country rent);
    print_string ("0, which is USD $");
    print_float rent;
    print_endline "0.";
    if country_idx <> 0 then begin
      print_string "You also paid an exchange fee of USD $";
      print_float exchange_fee;
      print_endline "0 to the bank.\n";
    end
    else ();
    try (* If the current player has enough money to pay the rent *)
      (let curr_player' = Player.change_wealth curr_player 
           ( -.rent -. exchange_fee) in
       let paid_player' = Player.change_wealth paid_player 
           (+. rent) in
       let bank' = deposit exchange_fee state.bank in
       state.players.(curr_player_id) <- curr_player';
       state.players.(owner_id) <- paid_player';
       state.bank <- bank';
       print_string"You now have USD $";
       print_float (Player.get_player_money curr_player'); print_endline"0.";
       print_string"Player ";
       print_string (Player.get_player_name paid_player');
       print_string" now has USD $";
       print_float (Player.get_player_money paid_player'); print_endline"\n";)
    with Failure msg -> 
      (*Bankruptcy if the current player does not have enough money to pay the 
        rent *)
      print_endline msg;
      print_endline "You went bankrupt! You are out of the game...";
      let paid_player_name = get_player_name paid_player in
      print_endline 
        ("\nGiving all of your money and places to " ^ paid_player_name ^ 
         "...\n");
      make_current_player_inactive state;
      let paid_player' = 
        Player.change_wealth paid_player (+. get_player_money curr_player) in
      state.players.(owner_id) <- paid_player';
      print_string "Player ";
      print_string (get_player_name paid_player');
      print_string " now has USD $";
      print_float (get_player_money paid_player');
      print_endline "0.\n";
      let places' = transfer_places curr_player_id owner_id state.places in
      state.places <- places';
  end

(** The cost to develop land is 5% of land and increase rent by 5%*)
let develop_land state = 
  let player_index = state.current_player in 
  let player = state.players.(player_index) in 
  let place = state.places.(get_curr_pos player) in 
  let develop_value = 0.05 *. Place.get_value place in 
  let player_id = Player.get_id player in
  let owner_id = Place.get_ownership place in
  let country = state.countries.(get_country place) in
  let exchange_fee = exchange_fee_for country develop_value in
  if owner_id = player_id then
    let player' = Player.change_wealth player (-.develop_value-.exchange_fee) in 
    let place' = Place.change_ownership place (player_index)in 
    let place'' = Place.change_rent place' (1.05 *. (Place.get_rent place')) in
    let place''' = Place.change_land_value place'' 
        (1.05 *. (Place.get_value place'')) in 
    let bank' = Bank.deposit (develop_value+.exchange_fee) state.bank in 
    state.players.(player_index) <- player';
    state.bank <- bank';
    state.places.(get_curr_pos player) <- place''';
    print_endline 
      ("Congrats, you have successfully developed this land, " ^ 
       (get_place_name place''') ^ "!");
    print_string "Player ";
    print_string (get_player_name player');
    print_string " now has USD $";
    print_float (get_player_money player');
    print_endline "0.\n";
  else
    failwith "Sorry, you can't develop this place, because you don't own it.\n"

let turn state = 
  (*print_endline (string_of_int state.current_player); *)
  state.current_player <- ((state.current_player+1) mod 4 )

let get_curr_player_id state =
  state.current_player





