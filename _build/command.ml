type object_phrase = string list

type command = 
  | Money
  | Quit
  | Purchase
  | Develop
  | End
  | Rent

exception Empty

exception Malformed

(**[parse_strtolst str] helper function of parse 
   is parsed stringlist of string str*)
let parse_strtolst (str:string) :string list = 
  match str with 
  |"" -> raise(Empty)
  |_-> (String.split_on_char ' ' str)

(**[paarse_space lst acc] helper function of parse 
   is string list with space and empty string elts eliminated*)
let rec parse_space (lst: string list) (acc:string list) = 
  match lst with 
  |[] -> acc
  |h :: t -> if h = "" then parse_space t acc
    else parse_space t (h::acc)

(**[ismalformed lst] helper function of parse, is the string list if the
   command is not malformed, raise the error Malformed if it is malformed*)
let ismalformed (lst : string list) : string list =
  match lst with 
  |[] -> lst
  |h :: t -> if (h <> "roll" &&  h <> "money" && h <> "quit"&& h <> "purchase"
                 && h <> "end" && h <> "develop"
                ) 
             || (h = "quit" && t <> []) || 
             (h = "money" && t <> []|| (h = "purchase" && t <> [])
              || (h = "end" && t <> []) || (h = "develop" && t <> [])) 
    then raise (Malformed)
    else lst

(**[to_command lst]
   Make the string list of user inputs to a command *)
let to_command (lst : string list) : command = 
  match lst with 
  |[] -> raise(Empty)
  | h :: t -> 
    if h = "money" then Money
    else if h = "quit" then Quit 
    else if h = "purchase" then Purchase 
    else if h = "develop" then Develop
    else if h = "end" then End
    (* else if h = "build" then Build t*)
    (*else if h = "sell" then Sell t
      else if h = "inventory" then Inventory*)
    else if h = "rent" then Rent
    else raise (Malformed)


let parse str =
  if ((str |> parse_strtolst |> parse_space []) = []) then raise (Empty)
  (*else if (str |> parse_strtolst |> parse_space[] = ["score"]  )*)
  else
    parse_space (parse_strtolst str) [] |> List.rev|> ismalformed |> to_command