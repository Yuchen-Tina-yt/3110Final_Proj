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

let welcome state = 
  let places_arr = places_arr(state) in 
  let player_arr3 = player_arr state in 
  let player = (get_curr_player state player_arr3) in 
  let place = (Array.get places_arr (get_curr_pos player)) in 
  print_endline (get_player_name player);
  print_string "You are at place ";
  print_endline (get_place_name place)

let winornot player : bool = 
  if (get_player_money player) > 1500.0 then true else false

let rec explore st : unit =
  let player_arr1 = player_arr st in 
  let player = (get_curr_player st player_arr1) in 
  if winornot (player) 
  then (print_endline "Congrats! You win. Game ends, exit automatically."; 
        Stdlib.exit 0)
  else(
    print_endline 
      "       To purchase this land, enter purchase; \n
       to roll the die, enter roll; \n 
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
             print_endline "Congrats, you successfully purchased this land!\n";
           end
           with Failure msg -> print_endline msg;
         end
       | Develop -> begin
           try begin
             develop_land st;
             print_endline "Congrats, you have successfully developed this land!\n";
           end
           with Failure msg -> print_endline msg;
         end
       | Quit -> (print_endline "Goodbye, game terminates!\n";
                  Stdlib.exit 0)
       | Roll -> move_player st;
         let places_arr = places_arr(st) in 
         let player_arr3 = player_arr st in 
         let player = (get_curr_player st player_arr3) in 
         let place = (Array.get places_arr (get_curr_pos player)) in
         print_string "You are at place ";
         print_endline (get_place_name place);
         explore st
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


(** [play_game f] starts the adventure in file [f]. *)



let rec play_game state : unit =

  welcome state ;
  explore state ;
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


