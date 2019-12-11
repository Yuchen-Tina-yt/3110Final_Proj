module CurrencyMap = Map.Make(Int)
open CurrencyMap
open Money

type t = 
  {name: string; curr_pos: int; money: float Map.Make(Int).t ; 
   id: int; chance: string list; weapons: Weapon.t list}

let make_player (name:string) (curr_pos: int) (id: int ) 
    (weapons: (Weapon.t list)): t = 
  let initial_money_map = empty |> add 0 0. |> add 1 0. |> add 2 0. |> 
                          add 3 1000. |> add 4 0. |> add 5 0. |> add 6 0. |> 
                          add 7 0. |> add 8 0. |> add 9 0. in
  {name =  name; curr_pos =  curr_pos; money = initial_money_map; id = id; 
   chance = []; weapons = weapons}

let add_wealth (player:t) (money: Money.t) : t= 
  let currency_of_money = get_country_idx money in
  let current_wealth_in_currency = 
    find currency_of_money player.money in
  let amt_of_money = get_amount money in

  let new_money = current_wealth_in_currency +. amt_of_money in
  if new_money >= 0. then
    {name= player.name; curr_pos = player.curr_pos; 
     money = add currency_of_money new_money player.money;
     weapons = player.weapons; id = player.id; chance = player.chance}
  else
    failwith ("Sorry, " ^ player.name ^ " does not have enough money. ")

let add_weapon (player:t) (weapon: Weapon.t): t = 
  {name = player.name; curr_pos = player.curr_pos; 
   money = player.money;
   weapons = weapon :: player.weapons; id = player.id; 
   chance = player.chance}

let remove_weapon (player: t) (weapon: Weapon.t) : t=
  {name = player.name; curr_pos = player.curr_pos; 
   money = player.money;
   weapons = List.filter 
       (fun x -> x <>  weapon) player.weapons;
   id = player.id; chance = player.chance}

let move_player' (player: t) (step: int ) = 
  let step' = ((player.curr_pos + step) mod 10) in    
  {name = player.name; curr_pos = step'; 
   money = player.money; 
   weapons = player.weapons; id = player.id; chance = player.chance}

let get_id (player:t) = 
  player.id

let get_curr_pos player =
  player.curr_pos

let get_player_name player = 
  player.name

let get_player_money player =
  bindings player.money |> List.map (function
      | country_idx, value -> make_money country_idx value)

let get_player_money_specific_currency player country_idx =
  find country_idx player.money

let add_player_chance player card_name = 
  {name = player.name; curr_pos = player.curr_pos; 
   money = player.money; 
   weapons = player.weapons; id = player.id; 
   chance = card_name :: player.chance}

(** [remove_player_chance_helper card_name list] is [list] with the first 
    instance of [card_name] removed from [list]*)
let rec remove_player_chance_helper card_name = function
  | [] -> failwith "My Lord, you unfornately do not own this chance card 
           and cannot use it.\n"
  | h::t -> if h = card_name then t else 
      h::remove_player_chance_helper card_name t

let remove_player_chance player card_name =
  {name = player.name; curr_pos = player.curr_pos; money = player.money; 
   weapons = player.weapons; id =  player.id; 
   chance  = remove_player_chance_helper card_name player.chance}

let get_weapons player =
  player.weapons

let get_player_chance player =
  player.chance

let mutate_player_name (player: t) (new_name : string) = 
  {name = new_name; curr_pos = player.curr_pos; money = player.money; 
   weapons = player.weapons; id = player.id; chance = player.chance}