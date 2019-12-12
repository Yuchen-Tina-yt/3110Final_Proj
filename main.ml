open Place 
open Player 
open Country 
open Gamestate
open Command
open Money
open Design
open Weapon

exception Illegal 

(**[print_strlist] is prints [list] to the terminal*)
let rec print_strlist (list : string list) = 
  match list with 
  |[] -> ()
  | h::t -> 
    print_string h ; print_string "; " ; print_strlist t

(**[welcome state] prints the welcome message and the player's name, and also
   the place name that the player is currently on and its ASCII art *)
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

(**[winornot player] is true if the player has won, and false otherwise. *)
let winornot state : bool = 
  if List.length (get_inactive_players_ids state) = 3 then true else false

(**[land_option st currency owner_id place buy_price country_idx] is teh
   function that prints out messages to notify teh player which land they 
   land on after the roll, and prints out informatino relevant to the land.*)
let land_option st currency owner_id place buy_price country_idx= 
  if owner_id = (get_curr_player_id st) then begin
    ANSITerminal.print_string [ANSITerminal.red] ("Welcome home, my lord! 👑");
    ANSITerminal.print_string [ANSITerminal.red] 
      (get_place_name place ^ " is yours. \n");
    let develop_price = 0.05 *. buy_price in
    ANSITerminal.print_string [ANSITerminal.magenta] 
      ("My lord, you have the choice of developing the land.\n");
    ANSITerminal.print_string [ANSITerminal.magenta] 
      ("Develop Price: " ^ currency ^ string_of_float develop_price ^ "\n");end
  else begin
    if owner_id = -1 then 
      ANSITerminal.print_string [ANSITerminal.magenta] 
        "The country currently belongs to no men. \n"
    else ();
    if country_idx <> 0 then begin
      ANSITerminal.print_string [ANSITerminal.magenta] 
        ("Purchase Price: " ^ currency ^ string_of_float buy_price ^ "\n");end
    else begin
      ANSITerminal.print_string [ANSITerminal.magenta] 
        ("Purchase Price: " ^currency^string_of_float buy_price ^ "\n");end end

(**[roll st] mutates the state and the player after moving the player 
   to the place in the state that corresponds to the number of the rolled die,
   printing out information of how the player has moved. 
*)
let roll st = 
  ANSITerminal.print_string [ANSITerminal.black; ANSITerminal.on_yellow] 
    "\n Now the dice rolling~~~ 🎲";print_endline "";move_player st;
  let places_arr = places_arr(st) in let player = (get_curr_player st) in 
  let place = (Array.get places_arr (get_curr_pos player)) in
  let owner_id = (get_ownership place) in let buy_price = (get_value place) in
  let country_idx = get_country place in
  let () = ANSITerminal.print_string [ANSITerminal.green] ("Off we go 🛩 ") in 
  let () = ANSITerminal.print_string 
      [ANSITerminal.black; ANSITerminal.on_yellow] (Design.frame_seperater) in 
  let () = print_endline "" in 
  let () = ANSITerminal.print_string [ANSITerminal.magenta] 
      ("Welcome to " ^ get_place_name place ^ "") in 
  let () = Design.get_country_design country_idx in 
  let country = country_at_index st (country_idx) in
  let currency = currency_in country in
  ANSITerminal.print_string [ANSITerminal.green] "Current Player: ";
  print_endline (get_player_name player);
  ANSITerminal.print_string [ANSITerminal.green] "Current Country: ";
  print_endline (get_place_name place ^ "\n");
  land_option st currency owner_id place buy_price country_idx


(**[introduction] is teh introductino message for the game *)
let introduction = 
  "  My Lord, you now have the following options: \n
       Enter 'purchase' to purchase the place \n 
       Enter 'develop' to develop the place \n
       Enter 'money' to check your iron bank reserve \n
       Enter 'quit' to forever rest in peace \n
       Enter 'end' to end your turn \n
       Enter 'chance' to see your chance cards \n
       Enter 'use free land' to use your chance card \n
       Enter 'buy_weapon' to buy a random weapon of a random price \n
       Enter 'weapons' to see your weapon arsenal \n"


(**[play_purchase st] is the function that purchases the land and mutates 
   teh state. It also prints out relevant messages in teh terminal to notify 
   the player. If exception raises we let the player continue play the game.
   This function is called when player input Purchase command.*)
let rec play_purchase st= 
  try begin 
    purchase st; 
    print_endline "You can develop this land when you next visit. ";
    print_endline "Your turn will now end. \n";
  end
  with Failure msg -> print_endline msg; play_round st

(**[play_develop st] is teh function that develops the land and mutates the 
   state. When exception raises it prints out messages in terminal to notify 
   the player and let teh player continue play the game. This function is 
   called when player input Develop command. *)
and play_develop st =  
  try begin
    develop_land st;
  end
  with Failure msg -> print_endline msg; play_round st

(**[play_quit st] is the function that let the player quit teh game. It also
   print out messages in teh terminal to notify teh player that they have quit 
   the game. This function is called when the player input Quit command.*)
and play_quit st = 
  make_current_player_inactive st; transfer_places st (-1);
  ANSITerminal.print_string [ANSITerminal.red] 
    "You have quit the game.\n You will be missed dearly. 💕 \n\n"

(**[play_money st] is teh function that print out messages in teh terminal
   to notify the player how much money in each currency do they have. This 
   function is called when the player input Money command.*)
and play_money st =
  let player = (get_curr_player st) in 
  print_string ("You have ");
  print_string (money_string st (get_player_money player)); 
  print_endline "0."; 
  play_round st 

(**[play_chance st] is teh function that prints put messages in the terminal
   to display all teh chance cards teh player is currently holding. This
   function is called when the player input Chance command. *)
and play_chance st =
  let player = (get_curr_player st) in
  let cards = get_player_chance player in
  if List.length cards = 0 then begin 
    ANSITerminal.print_string [ANSITerminal.blue]  
      "My Lord, you unfornately don't have any chance cards currently.\n";
    play_round st end
  else begin
    ANSITerminal.print_string [ANSITerminal.blue] 
      "My Lord, here are your chance cards: ";
    print_strlist cards;
    print_endline "";
    play_round st
  end

(**[play_use st object_phrase] is the function that let the player use 
   their free land chance card. If exception raises, this function prints
   put teh error message and let the player continue play. This function
   is called when the player input Use command.*)
and play_use st object_phrase = 
  try
    use_chance_card st object_phrase "free land";
  with Failure msg -> ANSITerminal.print_string [ANSITerminal.blue] msg;
    play_round st

(**[play_buy_weapon st] is teh function that let the player buy weapon
   and modifies the game state accordingly. If exception raises, it prints out 
   the error message and let the player continue play the game. It is called
   when the player input Buy_weapon command. *)
and play_buy_weapon st = 
  try (player_get_weapon st) with Failure msg -> print_endline msg; 
    play_round st

(**[play_weapons st] is the function taht let the player see their weapons 
   by printing out messages in the terminal. It is called when the player input
   Weapons command. *)
and play_weapons st = 
  let player = (get_curr_player st) in
  let weapons = (get_weapons player) in
  if weapons = [] then begin 
    ANSITerminal.print_string [ANSITerminal.blue]
      "My Lord, you unfortunately don't have any weapons currently.\n"; 
    play_round st
  end
  else begin
    ANSITerminal.print_string [ANSITerminal.blue]
      ("My Lord, here are your weapons: " ^ weapon_string (weapons) 
       ^ "\n");
    play_round st
  end

(**[play_round st] is the function that lets the current player play a single 
   round where the player doesn't land on another player's spot. *)
and play_round st: unit = 
  ANSITerminal.print_string[ANSITerminal.green] introduction; print_string"> ";
  try 
    (let command = parse (read_line () ) in
     match command with 
     | Purchase ->  play_purchase st
     | Develop -> play_develop st
     | Quit -> play_quit st
     | Money  -> play_money st
     | End ->  print_endline "Your turn ends."
     | Chance -> play_chance st
     | Use object_phrase -> play_use st object_phrase
     | Buy_Weapon -> play_buy_weapon st
     | Weapons -> play_weapons st
     |Pay | Battle -> ANSITerminal.print_string [ANSITerminal.magenta] 
                        "Invalid command at this time."; play_round st)
  with | Malformed -> (ANSITerminal.print_string [ANSITerminal.magenta] 
                         "error: command Malformed."; play_round st)
       | Empty -> (ANSITerminal.print_string [ANSITerminal.magenta] 
                     "error: command is Empty."; play_round st )

(** [choose_introduction] is the text to be displayed in the situation where 
    the current player must choose to either pay the rent or battle. *)
let choose_introduction =
  "  My Lord, you now have the following options: \n
       Enter 'battle' to battle \n
       Enter 'money' to check your iron bank reserve \n
       Enter 'pay' to pay the rent \n
       Enter 'weapons' to see your weapons arsenal \n
       Enter 'chance' to see your chance cards \n
       Enter 'use free escape' to use your chance card \n
       Enter 'quit' to forever rest in peace\n"

(** [choose_money st] shows the player the money that they have in [st]. *)
let rec choose_money st =
  let player = (get_curr_player st) in 
  print_string ("You have ");
  print_string (money_string st (get_player_money player)); 
  print_endline "0."; 
  choose_rent_or_battle st

(** [choose_use st] modifies the state for when the current player chooses to 
    use the chance card of [object_phrase] in [st].*)
and choose_use st object_phrase= 
  try
    use_chance_card st object_phrase "free escape";
  with Failure msg -> ANSITerminal.print_string [ANSITerminal.blue] msg;
    choose_rent_or_battle st 

(** [choose_chance st] shows the player the chance cards that they have in 
      [st]. *)
and choose_chance st =
  let player = (get_curr_player st) in
  let cards = get_player_chance player in
  if List.length cards = 0 then begin 
    ANSITerminal.print_string [ANSITerminal.blue]  
      "My Lord, you unfornately don't have any chance cards currently.\n";
    choose_rent_or_battle st end
  else begin
    ANSITerminal.print_string [ANSITerminal.blue] 
      "My Lord, here are your chance cards: ";
    print_strlist cards;
    print_endline "";
    choose_rent_or_battle st
  end   

(** [choose_weapons st] shows the player the weapons that they have in [st]. *)
and choose_weapons st =
  let player = (get_curr_player st) in
  let weapons = (get_weapons player) in
  if weapons = [] then begin 
    ANSITerminal.print_string [ANSITerminal.blue]
      "My Lord, you unfortunately don't have any weapons currently.\n"; 
    choose_rent_or_battle st
  end
  else begin
    ANSITerminal.print_string [ANSITerminal.blue]
      ("My Lord, here are your weapons: " ^ weapon_string (weapons));
    choose_rent_or_battle st
  end

(** [choose_quit st] modifies the state for when the current player chooses to 
    quit in [st]. *)
and choose_quit st =
  make_current_player_inactive st;
  transfer_places st (-1); 
  ANSITerminal.print_string [ANSITerminal.red] 
    "You have quit the game.\n You will be missed dearly. 💕 \n\n";
  turn st; play_game st

(**[choose_rent_or_battle st] is the function that lets the current player 
   choose to pay the rent or participate in battle *)
and choose_rent_or_battle st = 
  ANSITerminal.print_string [ANSITerminal.green] choose_introduction;
  print_string  "> ";
  try 
    (let command = parse (read_line () ) in
     match command with 
     | Purchase | Develop | End | Buy_Weapon -> 
       print_endline "Invalid command at this time."; choose_rent_or_battle st;
     | Money -> choose_money st
     | Use object_phrase -> choose_use st object_phrase
     | Pay -> ANSITerminal.print_string [ANSITerminal.magenta] 
                "You have decided to pay the rent."; rent st false;
     | Battle -> ANSITerminal.print_string [ANSITerminal.magenta] 
                   "You have decided to battle."; battle st;
     | Chance -> choose_chance st
     | Weapons -> choose_weapons st
     | Quit -> choose_quit st)
  with | Malformed -> (print_endline "error: command Malformed."; 
                       choose_rent_or_battle st)
       | Empty -> (print_endline "error: command is Empty."; 
                   choose_rent_or_battle st )


(**[play_game state] is the function that allows the players to play the game*)
and 
  play_game state : unit =
  let curr_player = get_curr_player state in
  if not (List.mem (get_id curr_player) (get_inactive_players_ids state)) 
  then begin
    welcome state;
    roll state;
    if check_rent state then begin
      ANSITerminal.print_string [ANSITerminal.magenta] 
        "The country currently belongs to another man. \n";
      choose_rent_or_battle state; end else explore state;
  end
  else ANSITerminal.print_string [ANSITerminal.green] 
      ((get_player_name curr_player) ^ " is out. \n" ^ 
       "The game shall continue. Now moving on to the next player.\n");
  turn state;
  play_game state 

and 
  (**[explore st] allows the player explore the state and make commands. 
     The function mutates the state and the player according to the parsed
     user input commands *)
  explore st : unit = 
  if winornot (st) 
  then begin
    ANSITerminal.(
      print_string 
        [red]
        "\n\n Salute, My Lord. 
        Your now reign in supreme. 🍺 \n");
    Stdlib.exit 0
  end
  else begin
    Random.self_init ();
    let num = Random.int 5 in
    (* (print_int num;) *)
    if num = 1 then lottery st
    else play_round st
  end

and 
  (** [lottery_num_0 st lottery_num] is when the lottery system in the game 
      gives thee player money *)
  lottery_num_0 st = 
  let amt = (Random.float 500.)in 
  let new_money = make_money 0 amt in
  print_string("You will be given $");
  print_float(amt); print_endline("!");
  Card.get_lottery_pic 0;
  let curr_player = get_curr_player st in 
  let new_player =  add_wealth curr_player new_money in 
  change_player st new_player;

and 
  (** [lottery_num_0 st lottery_num] is when the lottery system in the game 
        makes the player lose money *)
  lottery_num_1 st =
  let curr_player = get_curr_player st in 
  let amt = -.(Random.float 10.) *. 0.01 *.
              get_player_money_specific_currency curr_player 0 in 
  print_string("You will lose $");
  print_float(amt); print_endline("!");
  Card.get_lottery_pic 1;
  let new_money = make_money 0 amt in
  let new_player =  add_wealth curr_player new_money in 
  change_player st new_player;

and 
  lottery_num_2 st =
  print_string("You will move forward a random step.");
  Card.get_lottery_pic 2;
  roll st;
  if check_rent st then
    choose_rent_or_battle st
  else ();
  play_round st;
and
  (**[lottery st] is when the lottery system in the game randomly give the 
     player money, makes the player lose money, movesthe player forward a random 
     step, or let the player participate in the chance card system. *)
  lottery st : unit = 
  ANSITerminal.(
    print_string [blue]
      "Boom! You have entered the lottery system! \n");
  let lottery_num = Random.int 10 in 
  if lottery_num = 0 then lottery_num_0 st
  else if lottery_num = 1 then lottery_num_1 st
  else if lottery_num = 2 then lottery_num_2 st
  else begin  (** this is the chance card system, players can hold multiple 
                  chance cards with them and use one when condition satisfied.*)
    print_string("You will be given a chance card!");
    let curr_player = get_curr_player st in 
    let chance_cards = ["free land"; "free escape"] in
    let chance_num = Random.int 2 in 
    let card = List.nth chance_cards (chance_num) in 
    let new_player = add_player_chance curr_player card in 
    Card.get_lottery_pic lottery_num;
    change_player st new_player;
    print_string "You currently hold the following chance cards: ";
    print_strlist  (get_player_chance (get_curr_player st));
  end

(**[play state] is the function that plays the game. It is called in main to
   activate the game. *)
let play state : unit = 
  name_players state;
  play_game state

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.resize 99 99;
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Lord of Feuds!\n");
  Design.get_dice_design 0;
  Random.self_init ();
  let state = make_state in
  play state


(* Execute the game engine. *)
let () = main ()