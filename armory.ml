open Player 
open Weapon

type t = {mutable weapons: (Weapon.t * int) array}

let make_armory =
  let dagger = Weapon.make_weapon "Dagger" 50 in 
  let sword = Weapon.make_weapon "Sword" 100 in
  let gun = Weapon.make_weapon "Gun" 150 in  
  let fireball = Weapon.make_weapon "Fireball" 200 in 
  let iceball = Weapon.make_weapon "IceBall" 250 in 
  let plasmaball = Weapon.make_weapon "Plasmaball" 300 in 
  let arr = [|(dagger, 11);(sword, 10); (gun, 7); (fireball, 5); 
              (iceball, 3); (plasmaball, 1)|] in
  {weapons = arr}

let change_weapon_amt t weapon =
  for i = 0 to (Array.length t.weapons) do 
    let (name, amt) = Array.get t.weapons i in 
    if ((name.weapon_type = weapon.weapon_type ) && (amt >= 1)) then
      t.weapons.(i) <- (name, amt-1) else 
      failwith ("There are no more " ^ weapon.weapon_type ^ "left")
  done

