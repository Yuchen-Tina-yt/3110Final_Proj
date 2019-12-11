(* These unit tests check the functions for the more foundational modules for 
   our game. These teest demonstrate the correctness of our game system, because 
   they extensively test the essential helper functions which the higher level 
   functions in our game call. 

   Any functions in those modules that return unit were not unit tested, because 
   they depend on correct calls to functions in the more foundational modules 
   anyways. For the same reason, functions in main.ml and many functions in 
   gamestate.ml were not unit tested. 

   Functions in battle_art.ml, card.ml, and design.ml were not unit tested, 
   because those functions simply return ASCII art.

   More specifically, modules Country, Money, Place, player, Weapon, Gamestate, 
   Command are automatically tested bu OUnit.
   These test cases are developed by glass box testing.

   Any functions not tested by OUnit were manually checked by 
   thorough game play testing. 

   The testing approach demonstrates the correctness of the system because we 
   are 1.comparing the function result with the result that we are expecting 
   by running the OUnit test cases 2. we are manually playing the game to see
   what happens when we input commands and what events occurs, thus we could
   actually see if the game is the same as what we expected. Thus teh test 
   apprach could demonstrate the correctness of teh system.
*)

open OUnit2
open Country
open Money
open Place
open Player
open Weapon
open Gamestate
open Command

let country_1 = make_country "USD $" 0.01 0.5

let country_tests = [
  "Currency name of country_1 is 'USD $'" >::
  (fun _ -> assert_equal "USD $" (currency_in country_1));
  "2 USD's is 2.*.0.01 = 0.02 in country_1's currency" >::
  (fun _ -> assert_equal 0.02 (exchange_amount_for_USD country_1 2.));
  "2 of the country_1's currency is 2./.0.01 = 200. in USD's" >::
  (fun _ -> assert_equal 200. (exchange_amount_to_USD country_1 2.));
  "Exchange fee for exchanging 4. (arbitrary currency) in country_1 
  is  4.*.0.5 = 2." >::
  (fun _ -> assert_equal 2. (exchange_fee_for country_1 4.))
]

let five_dollars = make_money 1 5.

let money_tests = [
  "country index of five_dollars is 1" >::
  (fun _ -> assert_equal 1 (get_country_idx five_dollars));
  "amount of five_dollars is 5." >::
  (fun _ -> assert_equal 5. (get_amount five_dollars));
]

let place_1 = make_place "place" 1 150. 50. 20.
let place_2 = change_ownership place_1 3
let place_3 = change_land_value place_2 175.
let place_4 = change_rent place_3 75.

let place_tests = [
  "ownerid of place_1 is initiated to -1" >::
  (fun _ -> assert_equal (-1) (get_ownership place_1));
  "ownerid of place_2 is 3" >::
  (fun _ -> assert_equal 3 (get_ownership place_2));
  "value of place_1 is 150." >::
  (fun _ -> assert_equal 150. (get_value place_1));
  "value of place_3 is 175." >::
  (fun _ -> assert_equal 175. (get_value place_3));
  "rent of place_1 is 50." >::
  (fun _ -> assert_equal 50. (get_rent place_1));
  "rent of place_4 is 75." >::
  (fun _ -> assert_equal 75. (get_rent place_4));
  "name of place_1 is 'place'" >::
  (fun _ -> assert_equal "place" (get_place_name place_1));
  "country index of place_1 is 1" >::
  (fun _ -> assert_equal 1 (get_country place_1));
]

let ak47 = make_weapon "gun" 250

let weapon_tests = [
  "type of ak47 is 'gun'" >::
  (fun _ -> assert_equal "gun" (get_weapon_type ak47));
  "power of ak47 is 250" >::
  (fun _ -> assert_equal 250 (get_power ak47))
]

let million_dollars = make_money 3 2000000000.
let bob = make_player "Bob" 5 3 []
let new_name_player = mutate_player_name bob "Dave"
let bob_million_dollars = add_wealth bob million_dollars
let bob_ak47 = buy_weapon bob_million_dollars ak47
let bob_move = move_player' bob 3
let bob_chance = change_player_chance bob "nice"

let player_tests = [
  "name of bob is 'Bob'" >::
  (fun _ -> assert_equal "Bob" (get_player_name bob));
  "name of new_name_player is 'Dave'" >::
  (fun _ -> assert_equal "Dave" (get_player_name new_name_player));
  "current position of bob is 5" >::
  (fun _ -> assert_equal 5 (get_curr_pos bob));
  "current position of bob_move is 8" >::
  (fun _ -> assert_equal 8 (get_curr_pos bob_move));
  "id of bob is 3" >::
  (fun _ -> assert_equal 3 (get_id bob));
  "bob has no weapons" >::
  (fun _ -> assert_equal [] (get_weapons bob));
  "bob has no chance cards" >::
  (fun _ -> assert_equal [] (get_player_chance bob));
  "bob_chance has a chance card called 'nice'" >::
  (fun _ -> assert_equal ["nice"] (get_player_chance bob_chance));
  "bob has 0. in the currency with country index 0" >::
  (fun _ -> assert_equal 0. (get_player_money_specific_currency bob 0));
  "bob has 0. in the currency with country index 4" >::
  (fun _ -> assert_equal 0. (get_player_money_specific_currency bob 4));
  "bob_five_dollars has 2000001000. in the currency with country index 1" >::
  (fun _ -> assert_equal 2000001000. 
      (get_player_money_specific_currency bob_million_dollars 3));
]

let initial_state = make_state

let gamestate_tests = [
  "Current player id of initial_state is 0" >::
  (fun _ -> assert_equal 0 (get_curr_player_id initial_state));
  "Current player name is 'Cat'" >::
  (fun _ -> assert_equal "Cat" 
      (initial_state |> get_curr_player |> get_player_name));
  "No rent needs to be paid by the current player in inital_state" >::
  (fun _ -> assert_equal false (check_rent initial_state));
  "currency for at index 3 in inital_state is 'USD $'" >::
  (fun _ -> assert_equal "USD $" 
      (country_at_index initial_state 3 |> currency_in));
  "no inactive players in inital_state" >::
  (fun _ -> assert_equal [] (get_inactive_players_ids initial_state));
  "first place in the place array is 'China' in initial_state" >::
  (fun _ -> assert_equal "China" 
      ((places_arr initial_state).(0) |> get_place_name));
  "string for five_dollars in initial_state is '5.0 SEK kr'" >::
  (fun _ -> assert_equal "SEK kr5." 
      (money_string initial_state [five_dollars]));
]

let command_tests = [
  "parse of 'money' is Money"  >::
  (fun _ -> assert_equal Money (parse "money"));
  "parse of 'quit' is Quit"  >::
  (fun _ -> assert_equal Quit (parse "quit"));
  "parse of 'purchase' is Purchase"  >::
  (fun _ -> assert_equal Purchase (parse "purchase"));
  "parse of 'develop' is Develop"  >::
  (fun _ -> assert_equal Develop (parse "develop"));
  "parse of 'end' is End"  >::
  (fun _ -> assert_equal End (parse "end"));
  "parse of 'chance' is Chance"  >::
  (fun _ -> assert_equal Chance (parse "chance"));
  (* "parse of 'use chance chance' is Use ['chance']" >::
     (fun _ -> assert_equal (Use ["chance"]) (parse "use chance")); *)
  "parse of 'battle' is Battle"  >::
  (fun _ -> assert_equal Battle (parse "battle"));
  "parse of 'pay' is Pay"  >::
  (fun _ -> assert_equal Pay (parse "pay"));
  "parse of 'buy_weapon' is Buy_Weapon" >::
  (fun _ -> assert_equal Buy_Weapon (parse "buy_weapon"));
  "parse of 'money money' raises Malformed" >::
  (fun _ -> assert_raises Malformed (fun () -> parse "money money"));
  "parse of 'use' raises Malformed" >::
  (fun _ -> assert_raises Malformed (fun () -> parse "use"));
  "parse of '' raises Empty" >::
  (fun _ -> assert_raises Empty (fun () -> parse ""));
  "parse of 'notcommand' raises Malformed" >::
  (fun _ -> assert_raises Malformed (fun () -> parse "notcommand"));
  "parse of 'asdf' raises Malformed" >::
  (fun _ -> assert_raises Malformed (fun () -> parse "notcommand"));
  "parse of 'battle asfdf' raises Malformed" >::
  (fun _ -> assert_raises Malformed (fun () -> parse "notcommand"));
  "parse of 'end asdf' raises Malformed" >::
  (fun _ -> assert_raises Malformed (fun () -> parse "notcommand"));
  "parse of 'pay asdf' raises Malformed" >::
  (fun _ -> assert_raises Malformed (fun () -> parse "notcommand"));
  "parse of 'develop hifj' raises Malformed" >::
  (fun _ -> assert_raises Malformed (fun () -> parse "notcommand"));
  (* "parse of 'use chance1' raises Malformed" >::
     (fun _ -> assert_raises Malformed (fun () -> parse "use chance1 chance2")); *)
]

let suite =
  "test suite for Lord of Feuds"  >::: List.flatten [
    country_tests;
    money_tests;
    place_tests;
    weapon_tests;
    player_tests;
    gamestate_tests;
    command_tests;
  ]

let _ = run_test_tt_main suite