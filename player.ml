open Item

type t = 
  {name: string; curr_pos: int; money: float; weapons: (Weapon.t list) ; id: int; }


let make_player (name:string) (curr_pos: int)
    (money:float) (weapons: Weapon.t list) (id: int ): t = 
  {name =  name; curr_pos =  curr_pos; money = money; weapons= weapons; id = id}

let change_wealth (player:t) (amt:float) : t= 
  let new_money = player.money +. amt in
  if new_money >= 0. then
    {name= player.name; curr_pos = player.curr_pos; money = new_money;
     weapons = player.weapons; id = player.id}
  else
    failwith ("Sorry, " ^ player.name ^ " does not have enough money. ")

let buy_weapon (player:t) (weapon: Weapon.t): t = 
  {name = player.name; curr_pos = player.curr_pos; 
   money = player.money;
   weapons = weapon :: player.weapons; id = player.id}

let remove_weapon (player: t) (weapon: Weapon.t) : t=
  {name = player.name; curr_pos = player.curr_pos; 
   money = player.money;
   weapons = List.filter 
       (fun x -> x <>  weapon) player.weapons;
   id = player.id;}

let move_player' (player: t) (step: int ) = 
  let step' = ((player.curr_pos + step) mod 16) in    
  {name = player.name; curr_pos = step'; 
   money = player.money; 
   weapons = player.weapons; id = player.id;}

let get_id (player:t) = 
  player.id

let get_curr_pos player =
  player.curr_pos

let get_player_name player =
  player.name

let get_player_money player =
  player.money
let get_weapons player =
  player.weapons


(*
(**[find_newpos_name player steps places] is the new place name of the player 
   after moving number of steps steps from the player's current position*)
let find_newpos_name (player: t) 
    (steps: int) (places : Places.places_lst) : string=
  let curr_pos = palyer.curr_pos in 
  let start_index = List.find (fun x -> x=curr_pos) places in 
  List.nth places ((start_index + steps) mod (List.length Places))

let move_pos (player: t) (steps: int) (places : Places.places_lst) : t= 
  {name = player.name; curr_pos = find_newpos_name player steps places;
   money = player.money; items = player.items}

*)
