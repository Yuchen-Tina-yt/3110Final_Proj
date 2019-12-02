open Item

type t = 
  {name: string; curr_pos: int; money: float; items: string list; id: int; }


let make_player (name:string) (curr_pos: int)
    (money:float) (items: string list) (id: int ): t = 
  {name =  name; curr_pos =  curr_pos; money = money; items= items; id = id}

let change_wealth (player:t) (amt:float) : t= 
  let new_money = player.money +. amt in
  if new_money >= 0. then
    {name= player.name; curr_pos = player.curr_pos; money = new_money;
     items = player.items; id = player.id}
  else
    failwith ("Sorry, " ^ player.name ^ " does not have enough money. ")


let add_item (player:t) (item: Item.t) : t=
  {name = player.name; curr_pos = player.curr_pos; 
   money = player.money +. get_item_value item;
   items = get_item_name item :: player.items; id = player.id}

let remove_item (player: t) (item: Item.t) : t=
  {name = player.name; curr_pos = player.curr_pos; 
   money = player.money -. get_item_value item;
   items = List.filter 
       (fun x -> x <> get_item_name item) player.items; id = player.id;}

let move_player' (player: t) (step: int ) = 
  let step' = ((player.curr_pos + step) mod 16) in    
  {name = player.name; curr_pos = step'; 
   money = player.money; 
   items = player.items; id = player.id;}

let get_id (player:t) = 
  player.id

let get_curr_pos player =
  player.curr_pos

let get_player_name player =
  player.name

let get_player_money player =
  player.money


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
