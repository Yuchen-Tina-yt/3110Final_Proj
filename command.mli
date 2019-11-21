type object_phrase
type command = 
  | Money
  | Quit
  | Purchase
  | End
exception Empty

exception Malformed

(**[parse str] is the command parsed from the original user input *)
val parse: string -> command