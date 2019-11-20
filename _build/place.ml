open Player

type t = {name:string; country: int ; value: float; owner: Player.t option; 
          num_houses: int; rent_payment: float option; 
          pass_payment: float option}

let make_place name country value owner num_houses rent_payment pass_payment =
  {name = name; country = country; value = value; owner = owner; 
   num_houses = num_houses; rent_payment = rent_payment;
   pass_payment=pass_payment}

let change_ownership t player =
  {name= t.name; country =t.country; value=t.value; owner=player; 
   num_houses=t.num_houses; rent_payment=
                              t.rent_payment; pass_payment=t.pass_payment}

let change_land_value t new_val = 
  {name= t.name; country=t.country; value = new_val; owner =t.owner; 
   num_houses=t.num_houses; rent_payment = 
                              t.rent_payment; pass_payment=t.pass_payment}
let get_value t =
  t.value
