open Place 
open Item 
open Player 
open Country 
open Bank 
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
  let player_arr3 = player_arr state in 
  let player = (get_curr_player state player_arr3) in 
  let place = (Array.get places_arr (get_curr_pos player)) in 
  print_string "Player Name: ";
  print_endline (get_player_name player);
  print_string "You are at place ";
  print_endline (get_place_name place)

(**[winprnot player] is true if the player wins, and false o.w. *)
let winornot player : bool = 
  if (get_player_money player) > 1500.0 then true else false

(**[explore st] allows the player explore the state and make commands. 
   The function mutates the state and the player according to the parsed
   user input commands *)
let rec explore st : unit =
  let player_arr1 = player_arr st in 
  let player = (get_curr_player st player_arr1) in 
  if winornot (player) 
  then (print_endline "Congrats! You win. Game ends, exit automatically."; 
        Stdlib.exit 0)
  else(
    print_endline 
      "       To purchase this land, enter purchase; \n 
       to see your money, enter money; \n
       to quit game, enter quit.\n
       to end your turn, enter end\n";
    print_string  "> ";
    try 
      (let command = parse (read_line () ) in
       match command with 
       | Purchase ->  begin 
           print_endline 
             "Congrats, you successfully purchased this land!\n";
           purchase st;
         end
       | Quit -> (print_endline "Goodbye, game terminates!\n";
                  Stdlib.exit 0)
       | Money  -> begin let player_arr2 = player_arr st in 
           let player = (get_curr_player st player_arr2) in 
           print_float (get_player_money player); print_endline ""; 
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
  print_endline "Automatically Roll the Die...";
  print_endline "";
  move_player st;
  let places_arr = places_arr(st) in 
  let player_arr3 = player_arr st in 
  let player = (get_curr_player st player_arr3) in 
  let place = (Array.get places_arr (get_curr_pos player)) in
  print_string "You are at place ";
  print_endline (get_place_name place)

(**[play_game state] is the function that allows the players to play the game*)
let rec play_game state : unit =
  welcome state ;
  roll state;
  explore state;
  turn state;
  play_game state



(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to International Monopoly!\n");
  let state = make_state in
  play_game state





(* Execute the game engine. *)
let () = main ()


