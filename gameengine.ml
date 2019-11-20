open Place 
open Item 
open Player 
open Country 
open Bank 
open Gamestate
exception Illegal 


let rec print_strlist (list : string list) = 
  match list with 
  |[] -> ()
  | h::t -> 
    print_string h ; print_string "; " ; print_strlist t

let welcome state player= 
  print_string (Gamestate.placearray[player.curr_pos] );
  if (List.mem (start_room adv)(items_roomsadv adv)) then 
    (print_string "Item in this room: ";
     print_endline (room_itemname_adv adv (start_room adv)))

let winornot player : bool = 
  if player.money > 1500.0 then true else false

let rec explore st adv : unit=
  if (winornot (items_rooms st) && (List.length (items_rooms st)<>0)&& 
      (List.length (inventory_names st) = 0))
  then (print_endline "Congrats! You win. Exit automatically."; Stdlib.exit 0)
  else(
    ANSITerminal.(print_string [green]
                    "\nContinue exploring!\n");
    print_endline "Please enter a new command:\n";
    print_string  "> ";
    try 
      (let command = parse (read_line () ) in
       match command with 
       |Quit -> (print_endline "Goodbye and see you next adventure!\n";
                 Stdlib.exit 0)
       |Score  -> (print_int (current_tot_score st); explore st adv)
       |Go object_phrase ->(

           if (not (valid_exit (String.concat " " object_phrase) adv st) )
           then (print_string "error: illegal, not valid exit. ";
                 explore st adv)
           else if islocked st (next_room adv(current_room_id st)
                                  (String.concat " " object_phrase) )
           then (print_endline "The door is locked.";
                 explore st adv)
           else
             (try 
                print_endline 
                  ((go_nextstate (String.concat " " object_phrase) adv st)
                   |> (state_curr_roomdes adv));

                let state = 
                  (go_nextstate (String.concat " " object_phrase) adv st) in (
                  if (item_in_room_curr_state state 
                        (current_room_id state) = "red key") 
                  then print_endline "The red key lit the whole room magically";
                  if (current_room_id state = "treasure room")
                  then (explore state adv)
                  else if 
                    (item_in_room_curr_state state (current_room_id state)) 
                    <> ""
                  then 
                    (print_string "Item in this room: ";
                     print_endline 
                       (item_in_room_curr_state state (current_room_id state));
                     explore state adv)
                  else (print_string "No item in this room.";
                        explore state adv))
              with 
              |Illegal-> print_string "error: illegal, not valid exit. ";
                if (List.length (next_rooms adv (current_room_id st))=0) then 
                  print_string "You're done with your adventure! 
                Type quit to quit";
                explore st adv))

       |Take object_phrase ->
         (try 
            explore 
              ((String.concat " " object_phrase) 
               |> take adv st |> getstate_fromresult) adv
          with |Illegal -> (print_string "error: illegal, not valid item. ";
                            explore st adv)
               |UnknownItem -> 
                 (print_string "error: illegal, unknown take item.";
                  explore st adv)
         )
       |Drop object_phrase ->
         (try 
            explore 
              ((String.concat " " object_phrase) 
               |> drop adv st |> getstate_fromresult) adv
          with |Illegal -> (print_string "error: illegal, not valid item. ";
                            explore st adv)
               |UnknownItem ->
                 (print_string "error: illegal, unknown drop item.";
                  explore st adv)

         )
       |Inventory -> (if (List.length (inventory_names st) = 0) then
                        print_endline "inventory is empty"
                      else 
                        print_strlist (inventory_names st);
                      explore st adv)

       |Use object_phrase -> (
           if not (List.mem (String.concat " " object_phrase) 
                     (inventory_names st))
           then (print_string
                   "Invalid key name. Please reenter another key name.\n";
                 explore st adv)
           else
             (
               print_endline
                 "Please enter the exit name of
              the corresponding room that you want to open.\n";
               print_string  "> ";
               let ex = read_line () in  (

                 if (not (valid_exit ex adv st) )then 
                   (print_endline "error: illegal, not valid exit";
                    explore st adv)
                 else
                   (try 
                      explore ( ex|>
                                next_room adv (current_room_id st)|>
                                use adv st (String.concat " " object_phrase)
                                |> getstate_fromresult)  adv 

                    with |Illegal -> (print_string 
                                        "error: illegal, cannot open. ";
                                      explore st adv)
                         |UnknownRoom exn ->(print_string 
                                               "error: illegal, unknown room.";
                                             explore st adv)
                   ))))

      )
    with |Malformed -> (print_endline "error: command Malformed."; 
                        explore st adv)

         |Empty -> (print_endline "error: command is Empty."; 
                    explore st adv))


(** [play_game f] starts the adventure in file [f]. *)

let rec play_game f : unit =
  if Filename.check_suffix f "json" then 
    let adv = f |> open_json |> from_json in
    (welcome adv;
     explore (initst_fromadv adv) adv;
    )
  else 
    (print_endline
       "Error: invalid adventure file name.
       Please reenter the name of the game file you want to load.\n";
     print_string  "> ";
     let input = read_line () in
     match input with
     | exception End_of_file -> ()
     | file_name -> play_game file_name)


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Text Adventure Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  let input = read_line () in
  (*if (Filename.check_suffix input "json")
    then *)
  match input with
  | exception End_of_file -> ()
  | file_name -> play_game file_name





(* Execute the game engine. *)
let () = main ()


