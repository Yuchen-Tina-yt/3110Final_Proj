open Place
open Item

type t = 
  {name: string; curr_pos: int; money: float; items: string list}


let make_player (name:string) (curr_pos: int)
    (money:float) (items: string list): t = 
  {name =  name; curr_pos =  curr_pos; money = money; items= items}

let change_wealth (player:t) (amt:float) : t= 
  {name= player.name; curr_pos = player.curr_pos; money = player.money +. amt;
   items = player.items}


let add_item (player:t) (item: Item.t) : t=
  {name = player.name; curr_pos = player.curr_pos; 
   money = player.money +. item.item_value;
   items = item.item_name :: player.items}

let remove_item (player: t) (item: Item.t) : t=
  {name = player.name; curr_pos = player.curr_pos; 
   money = player.money -.   item.item_value;
   items = List.filter (fun x -> x <> item.item_name) player.items}

let move_player (player: t) (step: int ) = 
  {name = player.name; curr_pos = player.curr_pos + step; 
   money = player.money; 
   items = player.items}


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
