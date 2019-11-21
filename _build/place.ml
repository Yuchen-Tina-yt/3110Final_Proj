open Player

type t = {name:string; country: int ; value: float; ownerindex: int; 
          num_houses: int; rent_payment: float option; 
          pass_payment: float option}

let make_place name country value rent_payment pass_payment =
  {name = name; country = country; value = value; ownerindex = 0; 
   num_houses = 0; rent_payment = rent_payment;
   pass_payment=pass_payment}

let get_ownership t =
  t.ownerindex

let change_ownership t playerindex =
  {name= t.name; country =t.country; value=t.value; ownerindex = playerindex; 
   num_houses=t.num_houses; rent_payment=
                              t.rent_payment; pass_payment=t.pass_payment}

let change_land_value t new_val = 
  {name= t.name; country=t.country; value = new_val; ownerindex =t.ownerindex; 
   num_houses=t.num_houses; rent_payment = 
                              t.rent_payment; pass_payment=t.pass_payment}
let get_value t =
  t.value

let get_place_name t =
  t.name