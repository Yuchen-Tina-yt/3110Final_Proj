module CurrencyMap = Map.Make(Int)
open CurrencyMap
open Money

(** The abstract type of values representing the player *)
type t = 
  {name: string; curr_pos: int; money: float Map.Make(Int).t ; 
   id: int; chance: string list; weapons: Weapon.t list}

(** [make_player name curr_pos money weapons] is the player who has the 
    attributes name [name], curr_pos [curr_pos], money [money], and weapons 
    [weapons] *)
let make_player (name:string) (curr_pos: int) (id: int ) 
    (weapons: (Weapon.t list)): t = 
  let initial_money_map = empty |> add 0 0. |> add 1 0. |> add 2 0. |> 
                          add 3 1000. |> add 4 0. |> add 5 0. |> add 6 0. |> 
                          add 7 0. |> add 8 0. |> add 9 0. in
  {name =  name; curr_pos =  curr_pos; money = initial_money_map; id = id; 
   chance = []; weapons = weapons}

(**[add_wealth player money] is [player] after adding to them [money].*)
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

let buy_weapon (player:t) (weapon: Weapon.t): t = 
  add_wealth {name = player.name; curr_pos = player.curr_pos; 
              money = player.money;
              weapons = weapon :: player.weapons; id = player.id; chance = player.chance}
    (make_money 0 (-.(float_of_int (Weapon.get_power weapon))))

let remove_weapon (player: t) (weapon: Weapon.t) : t=
  {name = player.name; curr_pos = player.curr_pos; 
   money = player.money;
   weapons = List.filter 
       (fun x -> x <>  weapon) player.weapons;
   id = player.id; chance = player.chance}

(**[move_player' player step] is the player after moving by [step] on the map *)
let move_player' (player: t) (step: int ) = 
  let step' = ((player.curr_pos + step) mod 10) in    
  {name = player.name; curr_pos = step'; 
   money = player.money; 
   weapons = player.weapons; id = player.id; chance = player.chance}


(**[get_id player] is the player's id *)
let get_id (player:t) = 
  player.id

(**[get_curr_pos player] is the position of the player *)
let get_curr_pos player =
  player.curr_pos

(**[get_player_name player] is the name of the player*)
let get_player_name player = 
  player.name

(**[get_player_money player] is the money list of the player *)
let get_player_money player =
  bindings player.money |> List.map (function
      | country_idx, value -> make_money country_idx value)

(**[get_player_money_specific_currency player country_idx] is the money of the 
   specific country index of the player *)
let get_player_money_specific_currency player country_idx =
  find country_idx player.money

(**[change_player_chance player str] is the player after changed his/her
   chance card inventory *)
let change_player_chance player str = 
  {name = player.name; curr_pos = player.curr_pos; 
   money = player.money; 
   weapons = player.weapons; id = player.id; chance = str :: player.chance}

let get_weapons player =
  player.weapons

let get_player_chance player =
  player.chance

let mutate_player_name (player: t) (new_name : string) = 
  {name = new_name; curr_pos = player.curr_pos; money = player.money; 
   weapons = player.weapons; id = player.id; chance = player.chance}