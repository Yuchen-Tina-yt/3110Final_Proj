open OUnit2
open Country
open Money
open Place
open Player

(* TODO: Add test plan *)

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

let five_dollars = make_money 0 5.

let money_tests = [
  "country index of five_dollars is 0" >::
  (fun _ -> assert_equal 0 (get_country_idx five_dollars));
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

let bob = make_player "Bob" 5 3 []
let new_name_player = mutate_player_name bob "Dave"

let player_tests = [
  "name of bob is 'Bob'" >::
  (fun _ -> assert_equal "Bob" (get_player_name bob));
  "name of new_name_player is 'Dave'" >::
  (fun _ -> assert_equal "Dave" (get_player_name new_name_player));
  "current position of bob is 5" >::
  (fun _ -> assert_equal 5 (get_curr_pos bob));
  "id of bob is 3" >::
  (fun _ -> assert_equal 3 (get_id bob));
  "bob has no weapons" >::
  (fun _ -> assert_equal [] (get_weapons bob));
  "bob has no chance cards" >::
  (fun _ -> assert_equal [] (get_player_chance bob));
  "bob has 1000. in the currency with country index 0" >::
  (fun _ -> assert_equal 1000. (get_player_money_specific_currency bob 0));
  "bob has 0. in the currency with country index 4" >::
  (fun _ -> assert_equal 0. (get_player_money_specific_currency bob 4));
]

let suite =
  "test suite for Feud of Lords"  >::: List.flatten [
    country_tests;
    money_tests;
    place_tests;
    player_tests;
  ]

let _ = run_test_tt_main suite