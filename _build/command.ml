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
  | Buy_Weapon
  | Weapons

exception Empty

exception Malformed

(**[parse_strtolst str] is the list of [str] split by spaces  
   Raises [Empty] if [str] is an empty string. *)
let parse_strtolst (str:string) :string list = 
  match str with 
  |"" -> raise(Empty)
  |_-> (String.split_on_char ' ' str)

(**[parse_space lst acc] is [lst] with the empty strings removed appended onto 
   [acc].*)
let rec parse_space (lst: string list) (acc:string list) = 
  match lst with 
  |[] -> acc
  |h :: t -> if h = "" then parse_space t acc
    else parse_space t (h::acc)

(**[ismalformed lst] is the [lst] if it represents a command that isn't 
   malformed.
   Raises [Malformed] if [lst] represents a malformed command.*)
let ismalformed (lst : string list) : string list =
  match lst with 
  |[] -> lst
  |h :: t -> if (h <> "roll" &&  h <> "money" && h <> "quit"&& h <> "purchase"
                 && h <> "end" && h <> "develop" && h <> "chance" && h <> "use"
                 && h <> "battle" && h <> "pay" && h <> "buy_weapon" && 
                 h <> "weapons"
                ) 
             || (h = "quit" && t <> []) || 
             (h = "money" && t <> []|| (h = "purchase" && t <> [])
              || (h = "end" && t <> []) || (h = "develop" && t <> [])) 
             || (h = "chance" && t <> [] || (h = "use" && List.length t <> 2) || 
                 (h = "battle" && t <> []) || (h = "pay" && t <> [])
                 || (h = "buy_weapon" && t <> []) || h = "weapons" && t <> [])
    then raise (Malformed)
    else lst

(**[to_command lst] is the command represented by [lst].
   Raises [Malformed] if the command represented by [lst] is malformed and 
   raises [Empty] if [lst] is empty. *)
let to_command (lst : string list) : command = 
  match lst with 
  |[] -> raise(Empty)
  | h :: t -> 
    if h = "money" then Money
    else if h = "quit" then Quit 
    else if h = "purchase" then Purchase 
    else if h = "develop" then Develop
    else if h = "end" then End
    else if h = "chance" then Chance
    else if h = "use" then Use t
    else if h = "battle" then Battle
    else if h = "pay" then Pay
    else if h = "buy_weapon" then Buy_Weapon
    else if h = "weapons" then Weapons
    else raise (Malformed)


let parse str =
  if ((str |> parse_strtolst |> parse_space []) = []) then raise (Empty)
  else
    parse_space (parse_strtolst str) [] |> List.rev|> ismalformed |> to_command