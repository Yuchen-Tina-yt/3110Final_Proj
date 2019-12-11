

type t = {weapon_type: string; power: int}

let make_weapon name power = 
  {weapon_type= name; power = power}

let get_weapon_type weapon=
  weapon.weapon_type

let get_power weapon =
  weapon.power 

let rec weapon_string = function
  | [] -> ""
  | [weapon] -> get_weapon_type weapon ^ " (" ^ string_of_int (get_power weapon) 
                ^ " Power)" 
  | weapon::t -> get_weapon_type weapon ^ " (" ^ string_of_int 
                   (get_power weapon) ^ " Power), "  ^  weapon_string t 