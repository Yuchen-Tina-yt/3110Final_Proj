open Place 
open Item 
open Player 
open Country 
open Gamestate
open Command
exception Illegal 

(**[print_strlist] is the function that prints the string list to the console*)
let rec print_strlist (list : string list) = 
  match list with 
  |[] -> ()
  | h::t -> 
    print_string h ; print_string "; " ; print_strlist t

(**[welcome state] prints the welcome messgae and the player's name, and also
   the place name that the player is currently on *)
let welcome state = 
  let places_arr = places_arr(state) in 
  let player = (get_curr_player state) in 
  let place = (Array.get places_arr (get_curr_pos player)) in 
  print_string "Current Player Name: ";
  print_endline (get_player_name player);
  print_endline ("You are at Place " ^ (get_place_name place) ^ ".")

(**[winprnot player] is true if the player wins, and false o.w. *)
let winornot state : bool = 
  let money = state |> get_curr_player |> get_player_money in 
  if ((get_money_list_total_USD_equiv state money) > 1500.0 
      || (List.length (get_inactive_players_ids state)) = 3)then true else false

(**[explore st] allows the player explore the state and make commands. 
   The function mutates the state and the player according to the parsed
   user input commands *)
let rec explore st : unit =
  if winornot (st) 
  then (print_endline ("Congrats! All other players have been eliminated. " ^ 
                       "You win! Game ends, exit automatically."); 
        Stdlib.exit 0)
  else(
    print_endline 
      "       To purchase this place, enter purchase; \n 
       to develop your place, enter develop.\n
       to see your money, enter money; \n
       to quit game, enter quit.\n
       to end your turn, enter end\n";
    print_string  "> ";
    try 
      (let command = parse (read_line () ) in
       match command with 
       | Purchase ->  begin 
           try begin 
             purchase st; 
             print_endline "You can develop this land when you next visit. ";
             print_endline "Your turn will now end. \n";
           end
           with Failure msg -> print_endline msg;
         end
       | Develop -> begin
           try begin
             develop_land st;
           end
           with Failure msg -> print_endline msg; explore st
         end
       | Quit -> make_current_player_inactive st;
         transfer_places st (-1);
         (* Currently, the player's money isn't put back into the bank. 
            Maybe that should be implemented. *) 
         print_endline "You have quit the game. \n";
       | Money  -> begin 
           let player = (get_curr_player st) in 
           print_string ("You have ");
           print_string (money_string st (get_player_money player)); 
           print_endline "0."; 
           explore st end
       | End ->  print_endline "Your turn ends.";

      )
    with | Malformed -> (print_endline "error: command Malformed."; 
                         explore st)

         | Empty -> (print_endline "error: command is Empty."; 
                     explore st ))



(**[roll st] mutates the state and the player after moving the player 
   to the place in the state corresponds to the number of the rolled die *)
let roll st = 
  print_endline "";
  print_endline "Automatically rolling the die...";
  print_endline "";
  move_player st;
  let places_arr = places_arr(st) in 
  let player = (get_curr_player st) in 
  let place = (Array.get places_arr (get_curr_pos player)) in
  let owner_id = (get_ownership place) in
  let buy_price = (get_value place) in
  let country_idx = get_country place in
  let country = country_at_index st (country_idx) in
  let currency = currency_in country in
  print_endline ("You are now at Place " ^ (get_place_name place) ^ ".");
  if owner_id = (get_curr_player_id st) then begin
    print_endline "You own this place.";
    let develop_price = 0.05 *. buy_price in
    if country_idx <> 0 then begin
      print_string ("\nThe price to develop this place is " ^ currency);
      print_float develop_price;
      print_endline "0.\n"
    end
    else begin
      print_string ("\nThe price to buy this place is " ^ currency);
      print_float buy_price; print_endline "0.\n"
    end
  end
  else begin
    if owner_id = -1 then print_endline "No one owns this place."
    else ();
    if country_idx <> 0 then begin
      print_string ("\nThe price to buy this place is " ^ currency);
      print_float buy_price;
      print_endline "0.\n"
    end
    else begin
      print_string ("\nThe price to buy this place is " ^ currency);
      print_float buy_price; print_endline "0.\n"
    end
  end

(**[play_game state] is the function that allows the players to play the game*)
let rec play_game state : unit =
  let curr_player = get_curr_player state in
  if not (List.mem (get_id curr_player) (get_inactive_players_ids state)) 
  then begin
    welcome state ;
    roll state;
    rent state;
    explore state;
  end
  else print_endline ("\nPlayer " ^ (get_player_name curr_player) ^ " has been " ^ 
                      "eliminated.\nSkipping to the next player...\n");
  turn state;
  play_game state



(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to International Monopoly!\n");
  Random.self_init ();
  let state = make_state in
  play_game state





(* Execute the game engine. *)
let () = main ()


