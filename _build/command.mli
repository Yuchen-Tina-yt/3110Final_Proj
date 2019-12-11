(**This module represents the commands that could be used in the game and 
   functions to parse user input to commands *)
type object_phrase = string list

(**The abstract type represent the object_phrase *)
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

(**Exception of Empty *)
exception Empty

(**Exception of Malformed *)
exception Malformed

(**[parse str] is the command parsed from the original user input *)
val parse: string -> command