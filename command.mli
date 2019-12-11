(**This module represents the commands that could be used in the game and 
   functions to parse user input to commands *)

(**The type that represents the object phrase *)
type object_phrase = string list

(**The type that represents the commands *)
type command = 
  | Money
  | Quit
  | Purchase
  | Develop
  | End
  | Chance
  | Use of object_phrase
  | Battle
  | Pay
  | Buy_Weapon
  | Weapons

(**Exception of Empty *)
exception Empty

(**Exception of Malformed *)
exception Malformed

(**[parse str] is the command parsed from the original user input [str]. *)
val parse: string -> command