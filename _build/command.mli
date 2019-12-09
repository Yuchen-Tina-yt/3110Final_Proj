type object_phrase = string list
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

exception Empty

exception Malformed

(**[parse str] is the command parsed from the original user input *)
val parse: string -> command