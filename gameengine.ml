open Place 
open Item 
open Player 
open Country 
open Bank 
open Gamestate
open Command
exception Illegal 


let rec print_strlist (list : string list) = 
  match list with 
  |[] -> ()
  | h::t -> 
    print_string h ; print_string "; " ; print_strlist t

let welcome state player= 
  let places_arr = places_arr(state) in 
  let place = (Array.get places_arr (player.curr_pos)) in 
  print_string "You are at place ";
  print_endline place.name

let winornot player : bool = 
  if player.money > 1500.0 then true else false

let rec explore st player : unit=
  if winornot (player) 
  then (print_endline "Congrats! You win. Game ends, exit automatically."; 
        Stdlib.exit 0)
  else(
    print_endline 
      "To purchase this land, enter purchase; \n
       to roll the die, enter roll; \n 
       to see your money, enter money; \n
       to quit game, enter quit.\n";
    print_string  "> ";
    try 
      (let command = parse (read_line () ) in
       match command with 
       | Purchase ->  begin print_endline 
             "Congrats, you successfully purchased this land!\n";
           purchase st;
           explore st player
         end
       | Quit -> (print_endline "Goodbye, game terminates!\n";
                  Stdlib.exit 0)
       | Roll -> move_player st
       | Money  -> (print_float (player.money); print_endline ""; 
                    explore st player)

      )
    with |Malformed -> (print_endline "error: command Malformed."; 
                        explore st player)

         |Empty -> (print_endline "error: command is Empty."; 
                    explore st player))


(** [play_game f] starts the adventure in file [f]. *)



let rec play_game state : unit =
  let player_arr = player_arr state in 
  let player = (get_curr_player state player_arr) in 
  welcome state player;
  explore state player;
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


