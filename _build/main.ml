open Place 
open Player 
open Country 
open Gamestate
open Command
open Money
open Design

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
  let place_id = get_country place in 
  let () = Design.get_country_design place_id in 
  ANSITerminal.print_string [ANSITerminal.green] "Current Player: ";
  print_endline (get_player_name player);
  ANSITerminal.print_string [ANSITerminal.green] "Current Country: ";
  print_endline (get_place_name place)

(**[winprnot player] is true if the player wins, and false o.w. *)
let winornot state : bool = 
  let money = state |> get_curr_player |> get_player_money in 
  if ((get_money_list_total_USD_equiv state money) > 1500.0 
      || (List.length (get_inactive_players_ids state)) = 3)then true else false

(**[roll st] mutates the state and the player after moving the player 
   to the place in the state corresponds to the number of the rolled die *)
let roll st = 
  print_endline "";
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.on_yellow] 
    "Now the dice rolling~~~ 😉";
  print_endline "";
  move_player st;
  let places_arr = places_arr(st) in 
  let player = (get_curr_player st) in 
  let place = (Array.get places_arr (get_curr_pos player)) in
  let owner_id = (get_ownership place) in
  let buy_price = (get_value place) in
  let country_idx = get_country place in
  let () = Design.get_country_design country_idx in 
  let country = country_at_index st (country_idx) in
  let currency = currency_in country in
  ANSITerminal.print_string [ANSITerminal.green] "Current Player: ";
  print_endline (get_player_name player);
  ANSITerminal.print_string [ANSITerminal.green] "Current Country: ";
  print_endline (get_place_name place ^ "\n");
  if owner_id = (get_curr_player_id st) then begin
    ANSITerminal.print_string [ANSITerminal.red] ("Welcome home, my lord! 👑");
    ANSITerminal.print_string [ANSITerminal.red] (get_place_name place ^ " is yours. \n");
    let develop_price = 0.05 *. buy_price in
    if country_idx <> 0 then begin
      ANSITerminal.print_string [ANSITerminal.magenta] ("My lord, you have the choice of developing the land at your pleasure.\n");
      ANSITerminal.print_string [ANSITerminal.magenta] 
        ("Develop Price: " ^ currency ^ string_of_float develop_price ^ "\n");

    end
    else begin
      ANSITerminal.print_string [ANSITerminal.magenta] ("You have the choice of buying the place.\n");
      ANSITerminal.print_string [ANSITerminal.magenta] 
        ("Purchase Price: " ^ currency ^ string_of_float buy_price ^ "\n");
    end
  end
  else begin
    if owner_id = -1 then 
      ANSITerminal.print_string [ANSITerminal.magenta] "The country currently belongs to no men. \n"
    else ();
    if country_idx <> 0 then begin
      ANSITerminal.print_string [ANSITerminal.magenta] 
        ("Purchase Price: " ^ currency ^ string_of_float buy_price ^ "\n");
    end
    else begin
      ANSITerminal.print_string [ANSITerminal.magenta] 
        ("Purchase Price: " ^ currency ^ string_of_float buy_price ^ "\n");
    end
  end


let rec play_round st: unit = 
  ANSITerminal.print_string [ANSITerminal.green]
    "  My Lord, you now have the following options: \n
       Enter purchase to purchase the place \n 
       Enter develop to develop the place \n
       Enter monety to check your iron bank reserve \n
       Enter quit to surrender\n
       Enter end to end your turn \n
       Enter chance to see your chance cards \n
       Enter use to use your chance card \n";
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
         with Failure msg -> print_endline msg; play_round st
       end
     | Quit -> make_current_player_inactive st;
       transfer_places st (-1);
       (* Currently, the player's money isn't put back into the bank. 
          Maybe that should be implemented. *) 
       ANSITerminal.print_string [ANSITerminal.red] "You have quit the game.\n You will be missed dearly. 💕 \n\n";
     | Money  -> begin 
         let player = (get_curr_player st) in 
         print_string ("You have $");
         print_string (money_string st (get_player_money player)); print_endline "0."; 
         play_round st end
     | End ->  print_endline "Your turn ends.";
     | Chance -> let player = (get_curr_player st) in
       let cards = get_player_chance player in
       if List.length cards = 0 then begin 
         print_endline "You don't have any chance cards currently.";
         play_round st end
       else begin
         print_string "Here are your chance cards: ";
         print_strlist cards;
         print_endline "";
         play_round st
       end
     | Use object_phrase -> 
       let player = (get_curr_player st) in
       let cards = get_player_chance player in
       let card_name = (String.concat " " object_phrase) in 

       if List.mem card_name cards then begin
         print_string " You used chance card ";
         print_string card_name;
         if card_name = "free place" then get_free_place st
       end
       else print_endline "You do not own this chance card. You cannot use it.";
       play_round st
    )
  with | Malformed -> (print_endline "error: command Malformed."; 
                       play_round st)

       | Empty -> (print_endline "error: command is Empty."; 
                   play_round st )

let lottery st : unit = 
  ANSITerminal.(
    print_string [blue]
      "Boom! You have entered the lottery system! \n");
  let lottery_num = Random.int 100 in 
  if lottery_num = 0 then begin     (**give money case *)
    let amt = (Random.float 500.)in 
    let new_money = make_money 0 amt in
    print_string("You will be given $");
    print_float(amt); print_endline("!");
    let curr_player = get_curr_player st in 
    let new_player =  add_wealth curr_player new_money in 
    change_player st new_player;
  end
  else if lottery_num = 1 then begin     (**lose money case *)
    let curr_player = get_curr_player st in 
    let amt = -.(Random.float 10.) *. 0.01 *.
                get_player_money_specific_currency curr_player 0 in 
    print_string("You will lose $");
    print_float(amt); print_endline("!");
    let new_money = make_money 0 amt in
    let new_player =  add_wealth curr_player new_money in 
    change_player st new_player;
  end
  else if lottery_num = 2 then begin     (**move forward case *)
    print_string("You will move forward a random step.");
    welcome st;
    roll st;
    rent st;
    play_round st;
  end
  else begin  (** chance cards*)
    print_string("You will be given a chance card!");
    let curr_player = get_curr_player st in 
    let chance_cards = ["free land"; "free weapon"] in
    let chance_num = Random.int 2 in 
    let card = List.nth chance_cards (chance_num) in 
    print_string card;
    let new_player = change_player_chance curr_player card in 
    change_player st new_player;
    print_string "You currently hold the following chance cards: ";
    print_strlist  (get_player_chance (get_curr_player st));
  end


(**[explore st] allows the player explore the state and make commands. 
   The function mutates the state and the player according to the parsed
   user input commands *)


let explore st : unit =
  if winornot (st) 
  then (
    ANSITerminal.(
      print_string 
        [red]
        "\n\n Salute, My Lord. 
        Your now reign in supreme. 🍺 \n");
    Stdlib.exit 0)
  else(
    Random.self_init ();
    let num = Random.int 2 in
    (* (print_int num;) *)
    if num = 1 then lottery st
    else play_round st
  )


(**[play_game state] is the function that allows the players to play the game*)
let rec play_game state : unit =
  let curr_player = get_curr_player state in
  if not (List.mem (get_id curr_player) (get_inactive_players_ids state)) 
  then begin
    welcome state;
    roll state;
    rent state;
    explore state;
  end
  else ANSITerminal.print_string [ANSITerminal.green] 
      ((get_player_name curr_player) ^ " is out. \n" ^ 
       "Now moving on to the next player.\n");
  turn state;
  play_game state

let play state : unit = 
  (* welcome state; *)
  play_game state


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.resize 99 99;
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Feud of Lords!\n");
  Design.get_dice_design 0;
  Random.self_init ();
  let state = make_state in
  play state





(* Execute the game engine. *)
let () = main ()