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
  let player = (get_curr_player state) in 
  let place = (Array.get places_arr (get_curr_pos player)) in 
  print_string "Current Player Name: ";
  print_endline (get_player_name player);
  print_endline ("You are at Place " ^ (get_place_name place) ^ ".")

(**[winprnot player] is true if the player wins, and false o.w. *)
let winornot player : bool = 
  if (get_player_money player) > 1500.0 then true else false

(**[explore st] allows the player explore the state and make commands. 
   The function mutates the state and the player according to the parsed
   user input commands *)
let rec explore st : unit =
  let player = (get_curr_player st) in 
  if winornot (player) 
  then (print_endline "Congrats! You win. Game ends, exit automatically."; 
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
             print_endline 
               "Congrats, you have successfully developed this land!\n";
           end
           with Failure msg -> print_endline msg; explore st
         end
       | Quit -> (print_endline "Goodbye, game terminates!\n";
                  Stdlib.exit 0)
       | Money  -> begin 
           let player = (get_curr_player st) in 
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
  print_endline "Automatically rolling the die...";
  print_endline "";
  move_player st;
  let places_arr = places_arr(st) in 
  let player = (get_curr_player st) in 
  let place = (Array.get places_arr (get_curr_pos player)) in
  print_endline ("You are now at Place " ^ (get_place_name place) ^ ".\n")

(**[play_game state] is the function that allows the players to play the game*)
let rec play_game state : unit =
  let curr_player = get_curr_player state in
  if is_active curr_player then begin
    welcome state ;
    roll state;
    rent state;
    explore state;
  end
  else print_endline ("Player " ^ (get_player_name curr_player) ^ "has been " ^ 
                      "eliminated. Skipping to the next player...");
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


