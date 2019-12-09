

type t = {weapon_type: string; power: int}

let make_weapon name power = 
  {weapon_type= name; power = power}

let get_weapon_type weapon=
  weapon.weapon_type

let get_power weapon =
  weapon.power 