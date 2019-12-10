open Player 
open Weapon

type t = {mutable weapons: (Weapon.t * int) list; count: int}

let make_armory =
  let dagger = Weapon.make_weapon "Dagger" 50 in 
  let sword = Weapon.make_weapon "Sword" 100 in
  let gun = Weapon.make_weapon "Gun" 150 in  
  let fireball = Weapon.make_weapon "Fireball" 200 in 
  let iceball = Weapon.make_weapon "Iceball" 250 in 
  let plasmaball = Weapon.make_weapon "Plasmaball" 300 in 
  let arr = [(dagger, 11);(sword, 10); (gun, 7); (fireball, 5); 
             (iceball, 3); (plasmaball, 1)] in
  {weapons = arr; count = 37}

let rec remove_weapon_helper lst weapon= 
  match lst with 
  | []-> failwith "There aren't anymore weapons of this type"
  | (a,b)::y -> if ((a = weapon) && (b > 0)) then
      (a, b-1)::y else (a,b)::(remove_weapon_helper y weapon)

let remove_weapon t weapon =
  let lst = t.weapons in 
  let new_lst = remove_weapon_helper lst weapon in 
  if t.count < 1 then failwith "There are no more weapons left" else 
    {weapons = new_lst; count = t.count -1}

let armory_get_weapon t = 
  if t.count < 1 then failwith "There are no more weapons left" else 
    let dagger = Weapon.make_weapon "Dagger" 50 in 
    let sword = Weapon.make_weapon "Sword" 100 in
    let gun = Weapon.make_weapon "Gun" 150 in  
    let fireball = Weapon.make_weapon "Fireball" 200 in 
    let iceball = Weapon.make_weapon "IceBall" 250 in 
    let plasmaball = Weapon.make_weapon "Plasmaball" 300 in 
    let w_type_lst = [dagger; sword; gun; fireball;iceball;plasmaball] in 
    let ran_int = Random.int (List.length (w_type_lst)) in 
    let weapon = List.nth w_type_lst ran_int in  
    weapon 


