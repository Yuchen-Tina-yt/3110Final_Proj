open Place
open Player 
open Country 
open Money
open Design
open Weapon

type t = {
  mutable players: Player.t array; 
  mutable places: Place.t array;
  countries: Country.t array; 
  mutable current_player: int;
  mutable inactive_players_ids: int list; 
  mutable armory: Armory.t 
}

let places_arr (state: t ) = state.places
let country_at_index state idx  = Array.get state.countries idx

let get_curr_player (state:t) : Player.t = 
  Array.get state.players state.current_player

let get_inactive_players_ids state = state.inactive_players_ids

let make_current_player_inactive state = 
  state.inactive_players_ids <- state.current_player :: state.inactive_players_ids

let rec money_string state = function
  | [] -> ""
  | [money] -> (currency_in state.countries.(get_country_idx money)) ^ 
               string_of_float (get_amount money)
  | money::t -> (currency_in state.countries.(get_country_idx money)) ^ 
                string_of_float (get_amount money) ^ "0, " ^ 
                (money_string state t)

let rec get_money_list_total_USD_equiv state = function
  | [] -> 0.
  | money::t -> (exchange_amount_to_USD state.countries.(get_country_idx money) 
                   (get_amount money)) +. 
                (get_money_list_total_USD_equiv state t)

let make_state = 
  let player1 = Player.make_player "Cat" 0  0 [] in 
  let player2 = Player.make_player "Bunny" 0  1 [] in 
  let player3 = Player.make_player "Dog" 0  2 [] in
  let player4 = Player.make_player "Camel" 0 3 [] in
  let player_array = [|player1; player2; player3; player4|] in 

  let place1 = Place.make_place "China" 0 700. 100. 15. in 
  let place2 = Place.make_place "Sweden" 1 950. 150. 15. in 
  let place3 = Place.make_place "Japan" 2 10800. 1600. 15. in 
  let place4 = Place.make_place "USA" 3 100. 15. 15. in 
  let place5 = Place.make_place "Canada" 4 150. 25. 15. in 
  let place6 = Place.make_place "Korea" 5 120000. 18000. 15. in 
  let place7 = Place.make_place  "France" 6 100. 15. 15. in
  let place8 = Place.make_place  "Egypt" 7 1500. 225. 1. in
  let place9 = Place.make_place  "Australia" 8 150. 25. 15. in
  let place10 = Place.make_place  "Brazil" 9 400. 60. 15. in
  (* let place7 = Place.make_place "place7" 0 100. 15. 15. in 
     let place8 = Place.make_place "Place8" 1 100. 15. 15. in 
     let place9 = Place.make_place "place9" 2 100. 15. 15. in 
     let place10 = Place.make_place "place10" 3 100. 15. 15. in 
     let place11 = Place.make_place "place11" 4 100. 15. 15. in 
     let place12 = Place.make_place "place12" 5 100. 15. 15. in 
     let place13 = Place.make_place "place13" 0 100. 15. 15. in 
     let place14 = Place.make_place "place14" 1 100. 15. 15. in 
     let place15 = Place.make_place "place15" 2 100. 15. 15. in 
     let place16 = Place.make_place "place16" 3 100. 15. 15. in  *)
  (* let place_array = [|place1; place2; place3; place4; place5; place6; place7;
                      place8; place9; place10; place11; place12; place13; 
                      place14; place15; 
                      place16|] in  *)
  let place_array = [|place1; place2; place3; place4; place5; place6; place7; 
                      place8; place9; place10|] in
  let armory = Armory.make_armory in 


  let country_1 = Country.make_country "CNY ¥" 7.04 0.01 in 
  let country_2 = Country.make_country "SEK kr" 9.53 0.01 in
  let country_3 = Country.make_country "JPY ¥" 108.63 0.01 in
  let country_4 = Country.make_country "USD $" 1. 0.01 in
  let country_5 = Country.make_country "CAD $" 1.32 0.01 in
  let country_6 = Country.make_country "KRW ₩" 1193.02 0.01 in
  let country_7 = Country.make_country "EUR €" 0.9 0.01 in
  let country_8 = Country.make_country "EGP e£" 16.15 0.01 in
  let country_9 = Country.make_country "AUD A$" 1.45 0.01 in
  let country_10 = Country.make_country "BRL R$" 4.15 0.01 in
  let country_array = [|country_1; country_2; country_3; country_4; country_5; 
                        country_6; country_7; country_8; country_9; 
                        country_10|] in 
  let c_player = 0 in 

  {players = player_array; places = place_array; 
   countries = country_array; current_player = c_player; 
   inactive_players_ids = []; armory = armory}

let move_player state = 
  let step = (Random.int 6) + 1 in 
  let () = Design.get_dice_design step in 
  let player_int = state.current_player in 
  let player = state.players.(player_int) in 
  state.players.(player_int) <- Player.move_player' player step

(** [country_idx_of_most_money state idx max_amount money_list] is the 
    index of the country in state.countries that [money_list] has the most 
    money in. *)
let rec country_idx_of_most_money state idx max_amount = function 
  | [] -> idx
  | money::t -> let new_idx = money |> get_country_idx in
    let current_amount = 
      (money |> get_amount |> exchange_amount_to_USD 
         state.countries.(new_idx)) in 
    if current_amount > max_amount then
      country_idx_of_most_money state new_idx current_amount t 
    else
      country_idx_of_most_money state idx max_amount t

let pay amount state player country_idx country =
  (* First tries to pay that player in the local currency. *)
  try (add_wealth player 
         (make_money country_idx (-.amount))) with 
  (* Otherwise tries to pay that player in the currency which the player has the 
     most money in. *)
    Failure msg ->   
    let other_country_idx = 
      country_idx_of_most_money state 0 0. (get_player_money player) in
    let other_country = state.countries.(other_country_idx) in
    let other_country_currency = currency_in other_country in
    let this_country_currency = currency_in country in
    let exchange_amount = (amount |> exchange_amount_to_USD 
                             country |> exchange_amount_for_USD 
                             other_country) in
    let exchange_fee = exchange_fee_for other_country exchange_amount in
    print_endline ("Since you didn't have enough money in " ^ 
                   this_country_currency ^ ", you paid an exchange fee of " ^
                   other_country_currency ^ (string_of_float exchange_fee) ^ 
                   " and exchanged " ^ other_country_currency ^ 
                   (string_of_float exchange_amount) ^ "0 for " ^ 
                   this_country_currency ^ (string_of_float amount) ^ "0.\n");
    (add_wealth player (make_money other_country_idx 
                          (-.exchange_amount -.exchange_fee)))

let purchase state = 
  let player_index = state.current_player in 
  let player = state.players.(player_index) in 
  let place = state.places.(get_curr_pos player) in 
  let country_idx = get_country place in
  let country = state.countries.(country_idx) in
  let place_value = get_value place in
  if (get_ownership place = -1) then begin
    let player' = pay place_value state player country_idx country in
    let player_id = get_id player' in
    let place' = change_ownership place (player_id)in
    state.players.(player_index) <- player'; 
    state.places.(get_curr_pos player) <- place';
    ANSITerminal.print_string [ANSITerminal.magenta] 
      ("Congrats my lord, you successfully purchased this previously " ^ 
       "unowned land, " ^ (get_place_name place') ^ "!\n");
    ANSITerminal.print_string [ANSITerminal.magenta] 
      "You now have "; 
    ANSITerminal.print_string [ANSITerminal.magenta] 
      (money_string state (get_player_money player'));
    ANSITerminal.print_string [ANSITerminal.magenta] 
      "0.\n";
  end
  else if (get_ownership place =  player_index) then 
    failwith "My Lord, you have landed on the land you purchased previously. 
    Please enter another command.\n"
  else begin
    (* Buying the place from another player. 
       Right now, the old owner 
       doesn't get a choice whetheer or not they allow the current player to buy 
       the place from them. *)
    failwith "Sorry, the land belongs to some other Lord. 
    Therefore, the purchase cannot be completed.\n"
  end

(**changes the current player *)
let change_player state new_player = 
  let player_index = state.current_player in 
  state.players.(player_index) <- new_player

let transfer_places state receiver =
  state.places <-
    Array.map (fun h -> if get_ownership h = state.current_player
                then change_ownership h receiver
                else h) state.places

(** [transfer_wealth] *)
let rec transfer_wealth receiver = function
  | [] -> receiver
  | money::t -> transfer_wealth (add_wealth receiver money) t

let check_rent state = 
  let curr_player_id = state.current_player in 
  let curr_player = state.players.(curr_player_id) in 
  let place = state.places.(get_curr_pos curr_player) in
  let country_idx = get_country place in 
  let country = state.countries.(country_idx) in
  let owner_id = Place.get_ownership place in
  let currency = currency_in country in
  if ((owner_id = -1) ||  (owner_id = curr_player_id)) then false
  else begin 
    let paid_player = state.players.(owner_id) in
    let rent = Place.get_rent place in
    ANSITerminal.print_string [ANSITerminal.magenta] 
      "My Lord, you just arrived the land of Lord ";
    ANSITerminal.print_string [ANSITerminal.magenta] 
      (Player.get_player_name paid_player);
    ANSITerminal.print_string [ANSITerminal.magenta] 
      (".\nYou are charged with a rent of " ^ currency ^ string_of_float rent);
    ANSITerminal.print_string [ANSITerminal.magenta] "0.\n";
    true 
  end

let rent state is_higher = begin
  let curr_player_id = state.current_player in 
  let curr_player = state.players.(curr_player_id) in 
  let place = state.places.(get_curr_pos curr_player) in
  let country_idx = get_country place in 
  let country = state.countries.(country_idx) in
  let owner_id = Place.get_ownership place in
  let paid_player = state.players.(owner_id) in
  let rent = if is_higher then 1.5*.(Place.get_rent place) 
    else Place.get_rent place in
  try (* If the current player has enough money to pay the rent *)
    (let curr_player' = pay rent state curr_player country_idx country in
     let paid_player' = Player.add_wealth paid_player 
         (make_money country_idx (+. rent)) in
     state.players.(curr_player_id) <- curr_player';
     state.players.(owner_id) <- paid_player';
     ANSITerminal.print_string [ANSITerminal.magenta] 
       "You now have ";
     ANSITerminal.print_string [ANSITerminal.magenta] 
       (money_string state (get_player_money curr_player'));
     ANSITerminal.print_string [ANSITerminal.magenta] "0.\n";
     ANSITerminal.print_string [ANSITerminal.magenta] 
       "Lord ";
     ANSITerminal.print_string [ANSITerminal.magenta] 
       (get_player_name paid_player');
     ANSITerminal.print_string [ANSITerminal.magenta] 
       " now has ";
     ANSITerminal.print_string [ANSITerminal.magenta] 
       (money_string state (get_player_money paid_player')); 
     ANSITerminal.print_string [ANSITerminal.magenta] "\n";)
  with Failure msg -> 
    (*Bankruptcy if the current player does not have enough money to pay the 
      rent *)
    print_endline msg;
    ANSITerminal.print_string [ANSITerminal.red]
      "My Lord, you are bankrupt.
      With all due respect, please see yourself out.";
    let paid_player_name = get_player_name paid_player in
    print_endline 
      ("\n All your property will now goto " ^ paid_player_name ^ 
       "\n");
    make_current_player_inactive state;
    let curr_player_wealth = get_player_money curr_player in 
    let paid_player' = transfer_wealth paid_player curr_player_wealth in
    state.players.(owner_id) <- paid_player';
    print_string "Player ";
    ANSITerminal.print_string [ANSITerminal.magenta] 
      (get_player_name paid_player');
    ANSITerminal.print_string [ANSITerminal.magenta] 
      " now has ";
    ANSITerminal.print_string [ANSITerminal.magenta] 
      (money_string state (get_player_money paid_player'));
    ANSITerminal.print_string [ANSITerminal.magenta] 
      "0.\n";
    transfer_places state owner_id;
end

let develop_land state = 
  let player_index = state.current_player in 
  let player = state.players.(player_index) in 
  let place = state.places.(get_curr_pos player) in 
  let develop_value = 0.05 *. Place.get_value place in 
  let player_id = Player.get_id player in
  let owner_id = Place.get_ownership place in
  let country_idx = get_country place in
  let country = state.countries.(country_idx) in
  if owner_id = player_id then
    let player' = pay develop_value state player country_idx country in
    let place' = Place.change_ownership place (player_index)in 
    let place'' = Place.change_rent place' (1.05 *. (Place.get_rent place')) in
    let place''' = Place.change_land_value place'' 
        (1.05 *. (Place.get_value place'')) in 
    state.players.(player_index) <- player';
    state.places.(get_curr_pos player) <- place''';
    ANSITerminal.print_string [ANSITerminal.magenta]
      ("Congrats My Lord, you have successfully developed this land, " ^ 
       (get_place_name place''') ^ "!");
    ANSITerminal.print_string [ANSITerminal.magenta] "Player ";
    ANSITerminal.print_string [ANSITerminal.magenta] 
      (get_player_name player');
    ANSITerminal.print_string [ANSITerminal.magenta] " now has ";
    ANSITerminal.print_string [ANSITerminal.magenta] 
      (money_string state (get_player_money player'));
    ANSITerminal.print_string [ANSITerminal.magenta] "0.\n";
  else
    failwith 
      "My Lord, you can't develop this place, because you don't own it.\n"

let turn state = 
  state.current_player <- ((state.current_player+1) mod 4 )

let get_curr_player_id state =
  state.current_player

let battle state = 
  let player_index = state.current_player in 
  let player = state.players.(player_index) in 
  let place_index = get_curr_pos player in
  let place = state.places.(place_index) in 
  let owner_index = Place.get_ownership place in 
  if (owner_index <> -1) then begin
    let owner = state.players.(owner_index) in 
    let num_weapons_1 = List.length (Player.get_weapons player) in
    let num_weapons_2 = List.length (Player.get_weapons owner) in 
    if num_weapons_1 = 0 then
      begin
        ANSITerminal.print_string [ANSITerminal.magenta] 
          "Shame! You are unarmed.";
        ANSITerminal.print_string [ANSITerminal.magenta]
          ("My Lord, you have lost and there is no holy ground for the loser. 
          You lost the battle and now needs to pay a 50% higher remedy.\n");
        rent state true;
      end
    else if num_weapons_2 = 0 then begin
      state.places.(place_index) <- change_ownership place player_index;
      ANSITerminal.print_string [ANSITerminal.magenta] 
        "Congrats My Lord! The battle is won. You now own the land.\n";
    end
    else
      let random_int = Random.int num_weapons_1 in 
      let random_int_2 = Random.int num_weapons_2 in 
      let weapon_1 = List.nth (Player.get_weapons player) random_int in 
      let weapon_2 = List.nth (Player.get_weapons owner) random_int_2 in
      let player' = Player.remove_weapon player weapon_1 in 
      let owner' = Player.remove_weapon owner weapon_2 in 
      state.players.(player_index) <- player'; 
      state.players.(owner_index) <- owner';  
      if (Weapon.get_power weapon_1 > Weapon.get_power weapon_2) then begin
        state.places.(place_index) <- change_ownership place player_index;
        ANSITerminal.print_string [ANSITerminal.magenta] 
          "Congrats My Lord! The battle is won. You now own the land.\n" 
      end
      else begin
        ANSITerminal.print_string [ANSITerminal.magenta]
          ("My Lord, you have lost and there is no holy ground for the loser. 
          You lost the battle and now needs to pay a 50% higher remedy.\n");
        rent state true;
      end
  end
  else ()

let player_get_weapon state = 
  let player_index = state.current_player in 
  let player = state.players.(player_index) in 
  let weapon = Armory.armory_get_weapon state.armory in 
  let armory' = Armory.armory_remove_weapon state.armory weapon in 
  let country_idx = get_curr_pos player in
  let country = state.countries.(country_idx) in
  let amount_to_pay = exchange_amount_for_USD country
      (float_of_int (Weapon.get_power weapon)) in
  let player_after_paying = pay amount_to_pay state 
      player country_idx country in
  let player_with_weapon = add_weapon player_after_paying weapon in 
  let () = Battle_art.get_weapon_design weapon in 
  state.armory <- armory'; 
  state.players.(player_index) <- player_with_weapon;
  ANSITerminal.print_string [ANSITerminal.magenta] 
    "You now have "; 
  ANSITerminal.print_string [ANSITerminal.magenta] 
    (money_string state (get_player_money player_with_weapon));
  ANSITerminal.print_string [ANSITerminal.magenta] 
    "0.\n"

let get_free_place state = 
  let player_index = state.current_player in 
  let player = state.players.(player_index) in 
  let place = state.places.(get_curr_pos player) in 
  if (get_ownership place = -1) then begin
    let place' = Place.change_ownership place (player_index)in 
    state.places.(get_curr_pos player) <- place';
    ANSITerminal.print_string [ANSITerminal.magenta]
      ("Congrats My Lord, you are now the owner of this previously " 
       ^ "unowned land " ^ (get_place_name place') ^ " for free!");
  end
  else 
    ANSITerminal.print_string [ANSITerminal.magenta] 
      "My Lord, you can only get an unowned land for free."

let rec name player num state =
  ANSITerminal.print_string [ANSITerminal.green]  
    ("Dear Lord " ^ (string_of_int num) ^ 
     ", please enter thy unique name: ");
  let new_name = read_line () in
  if String.trim new_name <> "" && not 
       (Array.exists (fun player -> get_player_name player = new_name) 
          state.players) then
    mutate_player_name player new_name
  else name player num state

let name_players state = 
  for x = 0 to ((Array.length state.players)-1) do
    state.players.(x) <- name (state.players.(x)) (x+1) state
  done

let use_chance_card state object_phrase expected_card_name =
  let player_idx = get_curr_player_id state in
  let player = state.players.(player_idx) in
  let card_name = (String.concat " " object_phrase) in 
  if card_name = expected_card_name then 
    let player_removed_card = remove_player_chance player card_name in
    ANSITerminal.print_string [ANSITerminal.blue] 
      ("My Lord, you used your chance card " ^ card_name ^ ".\n");
    state.players.(player_idx) <- player_removed_card; 
    if card_name = "free land" then
      get_free_place state
    else ();
  else if card_name <> "free escape" && card_name <> "free land" then
    failwith
      ("My Lord, " ^ card_name ^ "is not a name of a chance card.\n")
  else
    failwith
      ("My Lord, you unfortunately cannot use chance card " ^ card_name ^
       " at this time.\n") 
